package cn.tianyu.tins;

import cn.tianyu.tins.ast.*;
import cn.tianyu.tins.ast.expr.*;
import cn.tianyu.tins.ast.stmt.*;
import cn.tianyu.tins.symbol.StructSymbol;
import cn.tianyu.tins.symbol.SymbolTable;
import cn.tianyu.tins.type.Token;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

public class Parser {

    Lexer lexer;

    public Parser(String fileName) {
        lexer = new Lexer(fileName);
    }

    public TopDefNode parser() {
        // 扫描所有struct，用于判断是否类型转换表达式
        scanStruct();
        lexer.restart();

        TopDefNode top = new TopDefNode();

        // 先加载所有import语句
        if (lexer.peekIgnoreLineBreak().type == Token.Type.IMPORT) {
            top.importNodes.addAll(importStmts());
        }

        while (lexer.peekIgnoreLineBreak().type != Token.Type.END) {
            if (lexer.peekIgnoreLineBreak().type == Token.Type.STRUCT) {
                top.structDefNodes.add(structDef());
            } else if (lexer.lookAheadIgnoreLineBreak(3).type == Token.Type.OPEN_PARENTHESIS) {
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
        Token token = lexer.nextIgnoreLineBreak();
        while (token.type != Token.Type.END) {
            // 读取到struct
            while (token.type != Token.Type.STRUCT && token.type != Token.Type.END) {
                token = lexer.nextIgnoreLineBreak();
            }
            if (token.type == Token.Type.END)
                break;
            String structName = lexer.matchIgnoreLineBreak(Token.Type.IDENTIFIER).name;
            SymbolTable.addStruct(structName);
            token = lexer.peekIgnoreLineBreak();
        }
    }

    private boolean isLineEnd(Token.Type type) {
        return type == Token.Type.LINE_BREAK || type == Token.Type.END;
    }

    /**
     * import语句必须在文件头部，所以一次性加载
     */
    private List<ImportNode> importStmts() {
        List<ImportNode> list = new LinkedList<>();

        while (lexer.peekIgnoreLineBreak().type == Token.Type.IMPORT) {
            lexer.match(Token.Type.IMPORT);

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
        lexer.matchIgnoreLineBreak(Token.Type.STRUCT);
        structDefNode.name = lexer.matchIgnoreLineBreak(Token.Type.IDENTIFIER).name;
        lexer.matchIgnoreLineBreak(Token.Type.L_CURLY_BRACKET);

        while (lexer.peekIgnoreLineBreak().type != Token.Type.R_CURLY_BRACKET) {
            if (lexer.peekIgnoreLineBreak().type == Token.Type.FUNC) {
                structDefNode.funcDefNodes.add(funcDef());
            } else {
                structDefNode.varDefNodes.addAll(varDefs());
            }
        }
        lexer.matchIgnoreLineBreak(Token.Type.R_CURLY_BRACKET);
        return structDefNode;
    }

    private FuncDefNode funcDef() {
        FuncDefNode funcDefNode = new FuncDefNode();
        lexer.matchIgnoreLineBreak(Token.Type.FUNC);
        Token funcType = lexer.nextIgnoreLineBreak();
        // 检查函数类型是否合法
        if (!isVarType(funcType) && funcType.type != Token.Type.VOID)
            lexer.error(funcType, "func type can't be " + funcType.name);
        funcDefNode.funcType = funcType;
        funcDefNode.funcName = lexer.matchIgnoreLineBreak(Token.Type.IDENTIFIER).name;
        funcDefNode.paramNode = funParam();
        funcDefNode.stmts = funcBody();
        return funcDefNode;
    }

    /**
     * 一行代码可能定义多个变量
     */
    private List<VarDefNode> varDefs() {
        List<VarDefNode> list = new ArrayList<>();

        // 数组定义
        if (lexer.lookAheadIgnoreLineBreak(2).type == Token.Type.L_SQUARE_BRACKET) {
            // 一行只能定义一个数组
            list.add(arrDef());
            return list;
        }

        Token varType = lexer.nextIgnoreLineBreak();
        while (true) {
            VarDefNode varDefNode = new VarDefNode();
            varDefNode.varType = varType;
            varDefNode.isArr = false;
            varDefNode.varName = lexer.match(Token.Type.IDENTIFIER).name;
            Token label = lexer.peekIgnoreLineBreak();
            if (label.type == Token.Type.ASSIGN) {
                lexer.next();
                varDefNode.value = expr();
                label = lexer.peek();
            }

            if (isLineEnd(label.type)) {
                lexer.nextIgnoreLineBreak();
                list.add(varDefNode);
                break;
            } else if (label.type == Token.Type.COMMA) {
                lexer.nextIgnoreLineBreak();
            } else {
                //遇到其他字符, 退出
                list.add(varDefNode);
                break;
            }
            list.add(varDefNode);
        }
        return list;
    }

    private VarDefNode arrDef() {
        VarDefNode varDef = new VarDefNode();
        varDef.isArr = true;
        varDef.varType = lexer.nextIgnoreLineBreak();
        varDef.varName = lexer.match(Token.Type.IDENTIFIER).name;
        // lexer.match()
        return varDef;
    }

    /**
     * 判断token type是否变量类型
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
        lexer.matchIgnoreLineBreak(Token.Type.OPEN_PARENTHESIS);
        // 空函数参数
        if (lexer.peekIgnoreLineBreak().type == Token.Type.CLOSE_PARENTHESIS) {
            lexer.nextIgnoreLineBreak();
            return nodes;
        }

        while (true) {
            FuncParamNode funcParamNode = new FuncParamNode();
            funcParamNode.paramType = lexer.nextIgnoreLineBreak();
            if (!isVarType(funcParamNode.paramType)) {
                lexer.error(funcParamNode.paramType, "func param type can't be " + funcParamNode.paramType.type.name());
            }
            funcParamNode.paramName = lexer.matchIgnoreLineBreak(Token.Type.IDENTIFIER).name;
            Token label = lexer.nextIgnoreLineBreak();
            // 判断数组
            if (label.type == Token.Type.L_SQUARE_BRACKET) {
                funcParamNode.isArr = true;
                lexer.matchIgnoreLineBreak(Token.Type.R_SQUARE_BRACKET);
                label = lexer.nextIgnoreLineBreak();
            }

            if (label.type == Token.Type.CLOSE_PARENTHESIS) {
                nodes.add(funcParamNode);
                break;
            } else if (label.type == Token.Type.COMMA) {
                nodes.add(funcParamNode);
            } else {
                lexer.unexpectedToken(label);
            }
        }
        return nodes;
    }

    private List<StmtNode> funcBody() {
        lexer.matchIgnoreLineBreak(Token.Type.L_CURLY_BRACKET);
        List<StmtNode> stmtNodes = new LinkedList<>();
        while (lexer.peekIgnoreLineBreak().type != Token.Type.R_CURLY_BRACKET) {
            stmtNodes.add(stmt());
        }
        lexer.matchIgnoreLineBreak(Token.Type.R_CURLY_BRACKET);
        return stmtNodes;
    }

    private StmtNode stmt() {
        Token token = lexer.peekIgnoreLineBreak();
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
            return new BreakStmtNode();
        } else if (type == Token.Type.CONTINUE) {
            return new ContinueStmtNode();
        } else if (type == Token.Type.RETURN) {
            return new ReturnStmtNode(expr());
        }
        if (isVarType(token)) {
            return new VarDefStmtNode(varDefs());
        }
        return new ExprStmtNode(expr());
    }

    private IfStmtNode ifStmt() {
        IfStmtNode ifStmtNode = new IfStmtNode();
        lexer.matchIgnoreLineBreak(Token.Type.IF);
        lexer.matchIgnoreLineBreak(Token.Type.OPEN_PARENTHESIS);
        ifStmtNode.condition = expr();
        lexer.matchIgnoreLineBreak(Token.Type.CLOSE_PARENTHESIS);
        ifStmtNode.ifStmt = stmt();

        if (lexer.peekIgnoreLineBreak().type == Token.Type.ELSE) {
            lexer.matchIgnoreLineBreak(Token.Type.ELSE);
            ifStmtNode.elseStmt = stmt();
        }
        return ifStmtNode;
    }

    private WhileStmtNode whileStmt() {
        WhileStmtNode whileStmtNode = new WhileStmtNode();
        lexer.matchIgnoreLineBreak(Token.Type.WHILE);
        lexer.matchIgnoreLineBreak(Token.Type.OPEN_PARENTHESIS);
        whileStmtNode.condition = expr();
        lexer.matchIgnoreLineBreak(Token.Type.CLOSE_PARENTHESIS);
        whileStmtNode.stmt = stmt();
        return whileStmtNode;
    }

    private ForStmtNode forStmtNode() {
        ForStmtNode forStmt = new ForStmtNode();
        lexer.matchIgnoreLineBreak(Token.Type.FOR);
        lexer.matchIgnoreLineBreak(Token.Type.OPEN_PARENTHESIS);
        forStmt.init = expr();
        lexer.matchIgnoreLineBreak(Token.Type.SEMICOLON);
        forStmt.condition = expr();
        lexer.matchIgnoreLineBreak(Token.Type.SEMICOLON);
        forStmt.operation = expr();
        lexer.matchIgnoreLineBreak(Token.Type.CLOSE_PARENTHESIS);
        forStmt.stmt = stmt();
        return forStmt;
    }

    private SwitchStmtNode switchStmtNode() {
        SwitchStmtNode switchStmt = new SwitchStmtNode();
        lexer.matchIgnoreLineBreak(Token.Type.SWITCH);
        lexer.matchIgnoreLineBreak(Token.Type.OPEN_PARENTHESIS);
        switchStmt.condition = expr();
        lexer.matchIgnoreLineBreak(Token.Type.CLOSE_PARENTHESIS);

        lexer.matchIgnoreLineBreak(Token.Type.L_CURLY_BRACKET);
        Token token = lexer.peekIgnoreLineBreak();
        List<CaseStmtNode> caseStmtNodes = new LinkedList<>();
        while (token.type != Token.Type.R_CURLY_BRACKET) {
            if (token.type == Token.Type.CASE) {
                CaseStmtNode caseStmt = new CaseStmtNode();
                lexer.matchIgnoreLineBreak(Token.Type.CASE);
                // todo case应该取term而不是expr
                caseStmt.condition = expr();
                lexer.matchIgnoreLineBreak(Token.Type.COLON);
                // todo case可以处理多条语句
                caseStmt.stmt = stmt();
                if (lexer.peekIgnoreLineBreak().type == Token.Type.BREAK) {
                    caseStmt.isBreak = true;
                    lexer.nextIgnoreLineBreak();
                }
            } else if (token.type == Token.Type.DEFAULT) {
                lexer.matchIgnoreLineBreak(Token.Type.DEFAULT);
                lexer.matchIgnoreLineBreak(Token.Type.COLON);
                // todo default可以处理多条语句
                switchStmt.defaultStmt = stmt();
            } else {
                lexer.unexpectedToken(token);
            }
            token = lexer.peekIgnoreLineBreak();
        }
        return switchStmt;
    }

    //表达式，默认为赋值表达式
    private ExprNode expr() {
        ExprNode left = condExpr();
        Token label = lexer.peekIgnoreLineBreak();
        if (label.type.ordinal() >= Token.Type.ASSIGN.ordinal()
                && label.type.ordinal() <= Token.Type.RSH_ASSIGN.ordinal()) {
            Token.Type operator = lexer.nextIgnoreLineBreak().type;
            ExprNode rightExpr = expr();
            return new AssignExpr(left, operator, rightExpr);
        }
        return left;
    }

    private ExprNode condExpr() {
        ExprNode left = logicOrExpr();
        Token label = lexer.peekIgnoreLineBreak();
        if (label.type == Token.Type.QUESTION_MARK) {
            lexer.nextIgnoreLineBreak();
            ExprNode trueExpr = expr();
            lexer.matchIgnoreLineBreak(Token.Type.COLON);
            return new CondExpr(left, trueExpr, condExpr());
        }
        return left;
    }

    // 这是正常写法
    /*private ExprNode logicOrExpr() {
        ExprNode left = logicAndExpr();
        while (lexer.peekIgnoreLineBreak().type == Token.Type.OR) {
            lexer.nextIgnoreLineBreak();
            left = new LogicOrExpr(left, logicAndExpr());
        }
        return left;
    }*/

    //这是我的第一想法，用递归代替循环，应该没问题
    private ExprNode logicOrExpr() {
        ExprNode left = logicAndExpr();
        if (lexer.peekIgnoreLineBreak().type == Token.Type.OR) {
            lexer.nextIgnoreLineBreak();
            return new LogicOrExpr(left, logicOrExpr());
        }
        return left;
    }

    private ExprNode logicAndExpr() {
        ExprNode left = bitOrExpr();
        while (lexer.peekIgnoreLineBreak().type == Token.Type.AND) {
            lexer.nextIgnoreLineBreak();
            left = new LogicAndExpr(left, bitOrExpr());
        }
        return left;
    }

    private ExprNode bitOrExpr() {
        ExprNode left = bitXorExpr();
        while (lexer.peekIgnoreLineBreak().type == Token.Type.B_OR) {
            lexer.nextIgnoreLineBreak();
            left = new BitOrExpr(left, bitXorExpr());
        }
        return left;
    }

    private ExprNode bitXorExpr() {
        ExprNode left = bitAndExpr();
        while (lexer.peekIgnoreLineBreak().type == Token.Type.B_XOR) {
            lexer.nextIgnoreLineBreak();
            left = new BitXorExpr(left, bitAndExpr());
        }
        return left;
    }

    private ExprNode bitAndExpr() {
        ExprNode left = equalityExpr();
        while (lexer.peekIgnoreLineBreak().type == Token.Type.B_AND) {
            lexer.nextIgnoreLineBreak();
            left = new BitAndExpr(left, equalityExpr());
        }
        return left;
    }

    private ExprNode equalityExpr() {
        ExprNode left = relationExpr();
        while (lexer.peekIgnoreLineBreak().type == Token.Type.EQ
                || lexer.peekIgnoreLineBreak().type == Token.Type.NE) {
            left = new EqualityExpr(left, lexer.nextIgnoreLineBreak().type, relationExpr());
        }
        return left;
    }

    private ExprNode relationExpr() {
        ExprNode expr = shiftExpr();
        while (lexer.peekIgnoreLineBreak().type.ordinal() >= Token.Type.GT.ordinal()
                && lexer.peekIgnoreLineBreak().type.ordinal() <= Token.Type.LT.ordinal()) {
            expr = new RelationExpr(expr, lexer.nextIgnoreLineBreak().type, shiftExpr());
        }
        return expr;
    }

    private ExprNode shiftExpr() {
        ExprNode expr = addOrSubExpr();
        while (lexer.peekIgnoreLineBreak().type == Token.Type.LSH
                || lexer.peekIgnoreLineBreak().type == Token.Type.RSH) {
            expr = new ShiftExpr(expr, lexer.nextIgnoreLineBreak().type, addOrSubExpr());
        }
        return expr;
    }

    private ExprNode addOrSubExpr() {
        ExprNode expr = mulOrDivExpr();
        while (lexer.peekIgnoreLineBreak().type == Token.Type.ADD
                || lexer.peekIgnoreLineBreak().type == Token.Type.SUB) {
            expr = new AddOrSubExpr(expr, lexer.nextIgnoreLineBreak().type, mulOrDivExpr());
        }
        return expr;
    }

    private ExprNode mulOrDivExpr() {
        ExprNode left = suffixUnaryExpr();
        while (lexer.peekIgnoreLineBreak().type == Token.Type.MUL
                || lexer.peekIgnoreLineBreak().type == Token.Type.DIV) {
            left = new MulOrDivExpr(left, lexer.nextIgnoreLineBreak().type, suffixUnaryExpr());
        }
        return left;
    }

    private ExprNode suffixUnaryExpr() {
        ExprNode expr = prefixUnaryExpr();
        if (lexer.peekIgnoreLineBreak().type == Token.Type.INC
                || lexer.peekIgnoreLineBreak().type == Token.Type.DEC) {
            return new SuffixUnaryExpr(expr, lexer.nextIgnoreLineBreak().type);
        }
        return expr;
    }

    private ExprNode prefixUnaryExpr() {
        Token.Type labelType = lexer.peekIgnoreLineBreak().type;
        // 取反，取非，都是可递归的
        if (labelType == Token.Type.BIT_REVERSE || labelType == Token.Type.NOT) {
            return new PrefixUnaryExpr(lexer.nextIgnoreLineBreak().type, prefixUnaryExpr());
        } else if (labelType == Token.Type.INC // 这些操作符不可递归
                || labelType == Token.Type.DEC
                || labelType == Token.Type.SUB) {
            return new PrefixUnaryExpr(lexer.nextIgnoreLineBreak().type, factorExpr());
        } else if (labelType == Token.Type.OPEN_PARENTHESIS) {
            // 类型转换表达式
            Token token = lexer.lookAheadIgnoreLineBreak(1);
            if (isVarType(token) || isCastType(token)) {
                // 吞掉左括号
                lexer.next();
                // 吞掉token，即转换类型
                lexer.next();
                lexer.matchIgnoreLineBreak(Token.Type.CLOSE_PARENTHESIS);
                return new CastExpr(token, prefixUnaryExpr());
            }
        }
        return factorExpr();
    }

    private ExprNode factorExpr() {
        ExprNode expr = null;
        Token token = lexer.nextIgnoreLineBreak();
        if (token.type == Token.Type.NUMBER_VAL
                || token.type == Token.Type.STRING_VAL
                || token.type == Token.Type.DOUBLE_VAL
                || token.type == Token.Type.FLOAT_VAL
                || token.type == Token.Type.CHAR_VAL) {
            expr = new PrimaryExpr(token);
        } else if (token.type == Token.Type.IDENTIFIER) {
            // 函数调用不可换行
            if (lexer.peek().type == Token.Type.OPEN_PARENTHESIS) {
                expr = callFuncExpr(token.name);
            } else {
                expr = new IdentifierExpr(token.name);
            }
        } else if (token.type == Token.Type.OPEN_PARENTHESIS) {
            ExprNode innerExpr = expr();
            lexer.matchIgnoreLineBreak(Token.Type.CLOSE_PARENTHESIS);
            expr = new ParenthesisExpr(innerExpr);
        }
        // 处理文法递归
        if (expr != null) {
            List<FactorExpr.NextExpr> nextExprList = new ArrayList<>();
            while (true) {
                Token.Type labelType = lexer.peekIgnoreLineBreak().type;
                if (labelType == Token.Type.DOT) { // id.id.id.id
                    lexer.nextIgnoreLineBreak();
                    nextExprList.add(new FactorExpr.NextExpr(lexer.match(Token.Type.IDENTIFIER).name));
                } else if (labelType == Token.Type.L_SQUARE_BRACKET) { // id[expr][expr][expr]
                    lexer.nextIgnoreLineBreak();
                    nextExprList.add(new FactorExpr.NextExpr(expr()));
                    lexer.match(Token.Type.R_SQUARE_BRACKET);
                } else {
                    break;
                }
            }
            return new FactorExpr(expr, nextExprList);
        }
        lexer.error(token, "unexpected token " + token.type.name());
        return null;
    }

    public CallFuncExprNode callFuncExpr(String funcName) {
        // 函数调用不可换行
        lexer.match(Token.Type.OPEN_PARENTHESIS);
        List<ExprNode> params = new LinkedList<>();
        // 空函数参数
        if (lexer.peekIgnoreLineBreak().type == Token.Type.CLOSE_PARENTHESIS) {
            lexer.nextIgnoreLineBreak();
            return new CallFuncExprNode(funcName, params);
        }
        while (true) {
            ExprNode exprNode = expr();
            Token label = lexer.nextIgnoreLineBreak();
            params.add(exprNode);
            if (label.type == Token.Type.CLOSE_PARENTHESIS) {
                break;
            } else if (label.type != Token.Type.COMMA) {
                lexer.unexpectedToken(label);
            }
        }
        return new CallFuncExprNode(funcName, params);
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