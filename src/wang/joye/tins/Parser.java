package wang.joye.tins;

import wang.joye.tins.ast.AST;
import wang.joye.tins.ast.expr.*;
import wang.joye.tins.ast.node.*;
import wang.joye.tins.ast.node.stmt.*;
import wang.joye.tins.symbol.StructSymbol;
import wang.joye.tins.symbol.SymbolTable;
import wang.joye.tins.type.Token;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

public class Parser {

    Lexer lexer;

    public Parser(String fileName) {
        lexer = new Lexer(fileName);
    }

    public AST parser() {
        // 扫描所有struct，放入符号表，用于判断是否类型转换表达式
        scanStruct();
        lexer.restart();

        AST top = new AST();

        // 先加载所有import语句
        if (lexer.peek().type == Token.Type.IMPORT) {
            top.importNodes.addAll(importStmts());
        }

        while (lexer.peek().type != Token.Type.END) {
            if (lexer.peek().type == Token.Type.STRUCT) {
                top.structDefNodes.add(structDef());
            } else if (lexer.peek().type == Token.Type.FUNC) {
                top.funcDefNodes.add(funcDef());
            } else {
                top.varDefNodes.addAll(varDefs());
            }
        }

        return top;
    }

    /**
     * 扫描所有struct名称，放入符号表
     */
    public void scanStruct() {
        Token token = lexer.next();
        while (token.type != Token.Type.END) {
            // 读取到struct
            while (token.type != Token.Type.STRUCT && token.type != Token.Type.END) {
                token = lexer.next();
            }
            if (token.type == Token.Type.END)
                break;
            String structName = lexer.match(Token.Type.IDENTIFIER).name;
            SymbolTable.addStruct(structName);
            token = lexer.peek();
        }
    }

    private boolean isLineEnd(Token.Type type) {
        return type == Token.Type.SEMICOLON || type == Token.Type.END;
    }

    /**
     * import语句必须在文件头部，所以一次性加载
     */
    private List<ImportNode> importStmts() {
        List<ImportNode> list = new LinkedList<>();

        while (lexer.peek().type == Token.Type.IMPORT) {
            lexer.next();

            ImportNode importNode = new ImportNode();
            while (true) {
                Token id = lexer.match(Token.Type.IDENTIFIER);
                importNode.folders.add(id.name);
                Token label = lexer.next();
                if (isLineEnd(label.type))
                    break;
                else if (label.type != Token.Type.DOT)
                    lexer.unexpectedToken(label, Token.Type.DOT);
            }
            list.add(importNode);
        }
        return list;
    }

    private StructDefNode structDef() {
        StructDefNode structDefNode = new StructDefNode();
        lexer.match(Token.Type.STRUCT);
        structDefNode.name = lexer.match(Token.Type.IDENTIFIER).name;
        lexer.match(Token.Type.L_CURLY_BRACKET);

        // 解析结构体属性
        while (lexer.peek().type != Token.Type.R_CURLY_BRACKET) {
            structDefNode.fields.addAll(varDefs());
        }
        lexer.match(Token.Type.R_CURLY_BRACKET);
        return structDefNode;
    }

    private FuncDefNode funcDef() {
        FuncDefNode funcDefNode = new FuncDefNode();
        lexer.match(Token.Type.FUNC);
        Token funcType = lexer.next();
        // 检查函数类型是否合法
        if (!isVarType(funcType) && funcType.type != Token.Type.VOID)
            lexer.error(funcType, "func type can't be " + funcType.name);
        funcDefNode.funcType = funcType;
        funcDefNode.funcName = lexer.match(Token.Type.IDENTIFIER).name;
        if (funcDefNode.funcName.equals("main")) {
            // TODO main func
        }
        funcDefNode.paramNode = funParam();
        funcDefNode.bodyStmt = compoundStmt();
        return funcDefNode;
    }

    /**
     * 一行代码可能定义多个变量
     */
    private List<VarDefNode> varDefs() {
        List<VarDefNode> list = new ArrayList<>();

        // 数组定义
        if (lexer.lookAhead(1).type == Token.Type.L_SQUARE_BRACKET) {
            // 一行只能定义一个数组
            list.add(arrDef());
            return list;
        }

        Token varType = lexer.next();
        while (true) {
            VarDefNode varDefNode = new VarDefNode();
            varDefNode.varType = varType;
            varDefNode.varName = lexer.match(Token.Type.IDENTIFIER).name;
            Token label = lexer.peek();
            if (label.type == Token.Type.ASSIGN) {
                lexer.next();
                varDefNode.value = expr();
                label = lexer.peek();
            }

            // 吞掉label
            lexer.next();
            list.add(varDefNode);
            if (isLineEnd(label.type)) {
                break;
            } else if (label.type != Token.Type.COMMA) {
                lexer.unexpectedToken(label);
            }
        }
        return list;
    }

    private VarDefNode arrDef() {
        VarDefNode arrDef = new VarDefNode();
        arrDef.varType = lexer.next();
        while (lexer.peek().type == Token.Type.L_SQUARE_BRACKET) {
            lexer.next();
            // 数组维数可能为空
            if (lexer.peek().type == Token.Type.R_SQUARE_BRACKET) {
                arrDef.addDimensionLength(null);
            } else {
                arrDef.addDimensionLength(expr());
            }
            lexer.match(Token.Type.R_SQUARE_BRACKET);
        }
        arrDef.varName = lexer.match(Token.Type.IDENTIFIER).name;
        if (lexer.peek().type == Token.Type.ASSIGN) {
            lexer.next();
            arrDef.value = expr();
        }
        lexer.match(Token.Type.SEMICOLON);
        return arrDef;
    }

    /**
     * 判断token type是否变量类型
     * 1. 基本类型
     * 2. struct名称
     */
    private boolean isVarType(Token token) {
        if (token.type.ordinal() >= Token.Type.INT.ordinal() && token.type.ordinal() <= Token.Type.DOUBLE.ordinal())
            return true;
        if (token.type == Token.Type.IDENTIFIER) {
            // 检查符号表中有没有该变量
            for (StructSymbol structSymbol : SymbolTable.structSymbols) {
                if (structSymbol.name.equals(token.name))
                    return true;
            }
        }
        return false;
    }

    /**
     * 函数参数解析
     */
    private List<FuncParamNode> funParam() {
        List<FuncParamNode> nodes = new LinkedList<>();
        lexer.match(Token.Type.OPEN_PARENTHESIS);
        // 空函数参数
        if (lexer.peek().type == Token.Type.CLOSE_PARENTHESIS) {
            lexer.next();
            return nodes;
        }

        while (true) {
            FuncParamNode funcParamNode = new FuncParamNode();
            funcParamNode.paramType = lexer.next();
            if (!isVarType(funcParamNode.paramType)) {
                lexer.error(funcParamNode.paramType, "func param type can't be " + funcParamNode.paramType.type.name());
            }
            // 判断数组
            int dimensionLength = 0;
            while (lexer.peek().type == Token.Type.L_SQUARE_BRACKET) {
                lexer.next();
                lexer.match(Token.Type.R_SQUARE_BRACKET);
                dimensionLength++;
            }
            // 可变参数
            if (lexer.peek().type == lexer.lookAhead(1).type
                    && lexer.peek().type == lexer.lookAhead(2).type
                    && lexer.peek().type == Token.Type.DOT) {
                funcParamNode.variableArr = true;
            }

            funcParamNode.paramName = lexer.match(Token.Type.IDENTIFIER).name;
            funcParamNode.dimensionLength = dimensionLength;

            Token label = lexer.next();

            nodes.add(funcParamNode);
            if (label.type == Token.Type.CLOSE_PARENTHESIS) {
                break;
            } else if (label.type != Token.Type.COMMA) {
                lexer.unexpectedToken(label);
            }
        }
        return nodes;
    }

    private StmtNode stmt() {
        Token token = lexer.peek();
        Token.Type type = token.type;
        if (type == Token.Type.IF) {
            return ifStmt();
        } else if (type == Token.Type.WHILE) {
            return whileStmt();
        } else if (type == Token.Type.FOR) {
            return forStmtNode();
        } else if (type == Token.Type.SWITCH) {
            return switchStmtNode();
        } else if (type == Token.Type.BREAK) {
            lexer.next();
            lexer.match(Token.Type.SEMICOLON);
            return new BreakStmtNode();
        } else if (type == Token.Type.CONTINUE) {
            lexer.next();
            lexer.match(Token.Type.SEMICOLON);
            return new ContinueStmtNode();
        } else if (type == Token.Type.RETURN) {
            lexer.next();
            ReturnStmtNode returnStmt = new ReturnStmtNode();
            //return后不包含表达式
            if (lexer.peek().type == Token.Type.SEMICOLON) {
                lexer.next();
                return returnStmt;
            }
            returnStmt.expr = expr();
            lexer.match(Token.Type.SEMICOLON);
            return returnStmt;
        } else if (type == Token.Type.L_CURLY_BRACKET) {
            return compoundStmt();
        } else if (type == Token.Type.SEMICOLON) {
            lexer.next();
            return new EmptyStmtNode();
        }
        if (isVarType(token)) {
            return new VarDefStmtNode(varDefs());
        }
        // 如果不是任何关键字语句，也不是变量定义语句
        // 只剩下表达式语句
        StmtNode res = new ExprStmtNode(expr());
        lexer.match(Token.Type.SEMICOLON);
        return res;
    }

    private CompoundStmtNode compoundStmt() {
        CompoundStmtNode compoundStmtNode = new CompoundStmtNode();
        lexer.match(Token.Type.L_CURLY_BRACKET);
        while (lexer.peek().type != Token.Type.R_CURLY_BRACKET) {
            compoundStmtNode.stmts.add(stmt());
        }
        lexer.next();
        return compoundStmtNode;
    }

    private IfStmtNode ifStmt() {
        IfStmtNode ifStmtNode = new IfStmtNode();
        lexer.match(Token.Type.IF);
        lexer.match(Token.Type.OPEN_PARENTHESIS);
        ifStmtNode.condition = expr();
        lexer.match(Token.Type.CLOSE_PARENTHESIS);
        ifStmtNode.ifStmt = stmt();

        while (lexer.peek().type == Token.Type.ELSE) {
            lexer.match(Token.Type.ELSE);
            // else if语句
            if (lexer.peek().type == Token.Type.IF) {
                lexer.next();
                IfStmtNode.ElseIfStmt elseIfStmt = new IfStmtNode.ElseIfStmt();
                lexer.match(Token.Type.OPEN_PARENTHESIS);
                elseIfStmt.condition = expr();
                lexer.match(Token.Type.CLOSE_PARENTHESIS);
                elseIfStmt.stmt = stmt();
            } else { // 遇到else语句，则不再循环匹配else if
                ifStmtNode.elseStmt = stmt();
                break;
            }
        }
        return ifStmtNode;
    }

    private WhileStmtNode whileStmt() {
        WhileStmtNode whileStmtNode = new WhileStmtNode();
        lexer.match(Token.Type.WHILE);
        lexer.match(Token.Type.OPEN_PARENTHESIS);
        whileStmtNode.condition = expr();
        lexer.match(Token.Type.CLOSE_PARENTHESIS);
        whileStmtNode.stmt = stmt();
        return whileStmtNode;
    }

    private ForStmtNode forStmtNode() {
        ForStmtNode forStmt = new ForStmtNode();
        lexer.match(Token.Type.FOR);
        lexer.match(Token.Type.OPEN_PARENTHESIS);
        // 初始化条件为语句
        forStmt.init = stmt();
        // 条件表达式可能为空
        if (lexer.peek().type != Token.Type.SEMICOLON) {
            forStmt.condition = expr();
        }
        lexer.match(Token.Type.SEMICOLON);
        // 操作表达式可能为空
        if (lexer.peek().type != Token.Type.CLOSE_PARENTHESIS) {
            forStmt.operation = expr();
        }
        lexer.match(Token.Type.CLOSE_PARENTHESIS);
        forStmt.stmt = stmt();
        return forStmt;
    }

    private SwitchStmtNode switchStmtNode() {
        SwitchStmtNode switchStmt = new SwitchStmtNode();
        lexer.match(Token.Type.SWITCH);
        lexer.match(Token.Type.OPEN_PARENTHESIS);
        switchStmt.condition = expr();
        lexer.match(Token.Type.CLOSE_PARENTHESIS);

        lexer.match(Token.Type.L_CURLY_BRACKET);
        Token token = lexer.peek();
        List<CaseStmtNode> caseStmtNodes = new LinkedList<>();
        while (token.type != Token.Type.R_CURLY_BRACKET) {
            if (token.type == Token.Type.CASE) {
                CaseStmtNode caseStmt = new CaseStmtNode();
                lexer.match(Token.Type.CASE);
                // todo case应该取term而不是expr
                caseStmt.condition = expr();
                lexer.match(Token.Type.COLON);
                // todo case可以处理多条语句
                caseStmt.stmt = stmt();
                if (lexer.peek().type == Token.Type.BREAK) {
                    caseStmt.isBreak = true;
                    lexer.next();
                }
            } else if (token.type == Token.Type.DEFAULT) {
                lexer.match(Token.Type.DEFAULT);
                lexer.match(Token.Type.COLON);
                // todo default可以处理多条语句
                switchStmt.defaultStmt = stmt();
            } else {
                lexer.unexpectedToken(token);
            }
            token = lexer.peek();
        }
        return switchStmt;
    }

    //表达式，默认为赋值表达式
    private ExprNode expr() {
        ExprNode left = condExpr();
        Token label = lexer.peek();
        if (label.type.ordinal() >= Token.Type.ASSIGN.ordinal()
                && label.type.ordinal() <= Token.Type.RSH_ASSIGN.ordinal()) {
            Token.Type operator = lexer.next().type;
            ExprNode rightExpr = expr();
            return new AssignExpr(left, operator, rightExpr);
        }
        return left;
    }

    private ExprNode condExpr() {
        ExprNode left = logicOrExpr();
        Token label = lexer.peek();
        if (label.type == Token.Type.QUESTION_MARK) {
            lexer.next();
            ExprNode trueExpr = expr();
            lexer.match(Token.Type.COLON);
            return new CondExpr(left, trueExpr, condExpr());
        }
        return left;
    }

    //这是我的第一想法，用递归代替循环，应该没问题
    private ExprNode logicOrExpr() {
        ExprNode left = logicAndExpr();
        if (lexer.peek().type == Token.Type.OR) {
            lexer.next();
            return new LogicOrExpr(left, logicOrExpr());
        }
        return left;
    }

    private ExprNode logicAndExpr() {
        ExprNode left = bitOrExpr();
        while (lexer.peek().type == Token.Type.AND) {
            lexer.next();
            left = new LogicAndExpr(left, bitOrExpr());
        }
        return left;
    }

    private ExprNode bitOrExpr() {
        ExprNode left = bitXorExpr();
        while (lexer.peek().type == Token.Type.B_OR) {
            lexer.next();
            left = new BitOrExpr(left, bitXorExpr());
        }
        return left;
    }

    private ExprNode bitXorExpr() {
        ExprNode left = bitAndExpr();
        while (lexer.peek().type == Token.Type.B_XOR) {
            lexer.next();
            left = new BitXorExpr(left, bitAndExpr());
        }
        return left;
    }

    private ExprNode bitAndExpr() {
        ExprNode left = equalityExpr();
        while (lexer.peek().type == Token.Type.B_AND) {
            lexer.next();
            left = new BitAndExpr(left, equalityExpr());
        }
        return left;
    }

    private ExprNode equalityExpr() {
        ExprNode left = relationExpr();
        while (lexer.peek().type == Token.Type.EQ
                || lexer.peek().type == Token.Type.NE) {
            left = new EqualityExpr(left, lexer.next().type, relationExpr());
        }
        return left;
    }

    private ExprNode relationExpr() {
        ExprNode expr = shiftExpr();
        while (lexer.peek().type.ordinal() >= Token.Type.GT.ordinal()
                && lexer.peek().type.ordinal() <= Token.Type.LT.ordinal()) {
            expr = new RelationExpr(expr, lexer.next().type, shiftExpr());
        }
        return expr;
    }

    private ExprNode shiftExpr() {
        ExprNode expr = addOrSubExpr();
        while (lexer.peek().type == Token.Type.LSH
                || lexer.peek().type == Token.Type.RSH) {
            expr = new ShiftExpr(expr, lexer.next().type, addOrSubExpr());
        }
        return expr;
    }

    private ExprNode addOrSubExpr() {
        ExprNode expr = mulOrDivExpr();
        while (lexer.peek().type == Token.Type.ADD
                || lexer.peek().type == Token.Type.SUB) {
            expr = new AddOrSubExpr(expr, lexer.next().type, mulOrDivExpr());
        }
        return expr;
    }

    private ExprNode mulOrDivExpr() {
        ExprNode left = suffixUnaryExpr();
        while (lexer.peek().type == Token.Type.MUL
                || lexer.peek().type == Token.Type.DIV) {
            left = new MulOrDivExpr(left, lexer.next().type, suffixUnaryExpr());
        }
        return left;
    }

    private ExprNode suffixUnaryExpr() {
        ExprNode expr = prefixUnaryExpr();
        if (lexer.peek().type == Token.Type.INC
                || lexer.peek().type == Token.Type.DEC) {
            return new SuffixUnaryExpr(expr, lexer.next().type);
        }
        return expr;
    }

    private ExprNode prefixUnaryExpr() {
        Token.Type labelType = lexer.peek().type;
        // 取反，取非，都是可递归的
        if (labelType == Token.Type.BIT_REVERSE || labelType == Token.Type.NOT) {
            return new PrefixUnaryExpr(lexer.next().type, prefixUnaryExpr());
        } else if (labelType == Token.Type.INC // 这些操作符不可递归
                || labelType == Token.Type.DEC
                || labelType == Token.Type.SUB) {
            return new PrefixUnaryExpr(lexer.next().type, factorExpr());
        } else if (labelType == Token.Type.OPEN_PARENTHESIS) {
            // 类型转换表达式
            Token token = lexer.lookAhead(1);
            if (isVarType(token) || isCastType(token)) {
                // 吞掉左括号
                lexer.next();
                // 吞掉token，即转换类型
                lexer.next();
                lexer.match(Token.Type.CLOSE_PARENTHESIS);
                return new CastExpr(token, prefixUnaryExpr());
            }
        }
        return factorExpr();
    }

    private FactorExpr factorExpr() {
        FactorExpr expr = null;
        Token token = lexer.peek();
        if (token.type == Token.Type.NUMBER_VAL
                || token.type == Token.Type.STRING_VAL
                || token.type == Token.Type.DOUBLE_VAL
                || token.type == Token.Type.FLOAT_VAL
                || token.type == Token.Type.CHAR_VAL) {
            lexer.next();
            return new PrimaryExpr(token);
        } else if (token.type == Token.Type.L_CURLY_BRACKET) { // 结构体赋值
            return structExpr();
        } else if (token.type == Token.Type.L_SQUARE_BRACKET) { // 数组赋值
            return arrExpr();
        } else if (token.type == Token.Type.IDENTIFIER) {
            lexer.next();
            if (lexer.peek().type == Token.Type.OPEN_PARENTHESIS) { // 函数调用
                expr = callFuncExpr(token.name);
            } else {
                expr = new IdentifierExpr(token.name); // 变量调用
            }
        } else if (token.type == Token.Type.OPEN_PARENTHESIS) { //括号表达式
            lexer.match(Token.Type.OPEN_PARENTHESIS);
            ExprNode innerExpr = expr();
            lexer.match(Token.Type.CLOSE_PARENTHESIS);
            expr = new ParenthesisExpr(innerExpr);
        }
        if (expr == null) {
            lexer.error(token, "unexpected token " + token.type.name());
        }
        FactorExpr factorExpr = new FactorExpr(expr);
        List<ExprNode> arrIndexList = new LinkedList<>();
        // factor[0][1][2]
        while (lexer.peek().type == Token.Type.L_SQUARE_BRACKET) {
            lexer.next();
            arrIndexList.add(expr());
            lexer.match(Token.Type.R_SQUARE_BRACKET);
        }
        factorExpr.arrIndexList = arrIndexList;
        if (lexer.peek().type == Token.Type.DOT) {
            lexer.next();
            factorExpr.nextFactor = factorExpr();
        }
        return factorExpr;
    }

    public CallFuncExprNode callFuncExpr(String funcName) {
        // 函数调用不可换行
        lexer.match(Token.Type.OPEN_PARENTHESIS);
        List<ExprNode> params = new LinkedList<>();
        // 空函数参数
        if (lexer.peek().type == Token.Type.CLOSE_PARENTHESIS) {
            lexer.next();
            return new CallFuncExprNode(funcName, params);
        }
        while (true) {
            ExprNode exprNode = expr();
            Token label = lexer.next();
            params.add(exprNode);
            if (label.type == Token.Type.CLOSE_PARENTHESIS) {
                break;
            } else if (label.type != Token.Type.COMMA) {
                lexer.unexpectedToken(label);
            }
        }
        return new CallFuncExprNode(funcName, params);
    }

    public ArrExpr arrExpr() {
        lexer.match(Token.Type.L_SQUARE_BRACKET);
        ArrExpr arrExpr = new ArrExpr();
        if (lexer.peek().type == Token.Type.R_SQUARE_BRACKET) {
            lexer.next();
            return arrExpr;
        }
        while (true) {
            arrExpr.addValue(expr());
            Token label = lexer.next();
            if (label.type == Token.Type.R_SQUARE_BRACKET)
                break;
            else if (label.type != Token.Type.COMMA)
                lexer.unexpectedToken(label);
        }
        return arrExpr;
    }

    /**
     * 结构体赋值语句
     */
    public StructAssignExpr structExpr() {
        lexer.match(Token.Type.L_CURLY_BRACKET);
        StructAssignExpr structAssignExpr = new StructAssignExpr();
        if (lexer.peek().type == Token.Type.R_CURLY_BRACKET) {
            lexer.next();
            return structAssignExpr;
        }
        while (true) {
            StructAssignExpr.ObjectField field = new StructAssignExpr.ObjectField();
            // {field: 1}
            if (lexer.lookAhead(1).type == Token.Type.COLON) {
                field.name = lexer.match(Token.Type.IDENTIFIER).name;
                lexer.match(Token.Type.COLON);
            }
            field.expr = expr();
            Token label = lexer.next();
            if (label.type == Token.Type.R_CURLY_BRACKET)
                break;
            else if (label.type != Token.Type.COMMA)
                lexer.unexpectedToken(label);
        }
        return structAssignExpr;
    }

    /**
     * 判断token是否强转的类型
     * 即是否struct的name
     */
    public boolean isCastType(Token token) {
        if (token.type != Token.Type.IDENTIFIER) {
            return false;
        }
        for (StructSymbol structSymbol : SymbolTable.structSymbols) {
            if (structSymbol.name.equals(token.name))
                return true;
        }
        return false;
    }
}