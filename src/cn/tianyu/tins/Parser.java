package cn.tianyu.tins;

import cn.tianyu.tins.ast.*;
import cn.tianyu.tins.ast.stmt.*;
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
        TopDefNode top = new TopDefNode();

        // 先加载所有import语句
        if (lexer.peekIgnoreLineBreak().type == Token.Type.IMPORT) {
            top.importNodes.addAll(importStmts());
        }

        while (lexer.peekIgnoreLineBreak().type != Token.Type.END) {
            if (lexer.peekIgnoreLineBreak().type == Token.Type.STRUCT) {
                top.structDefNodes.add(structDef());
            } else if (isFunc()) {
                top.funcDefNodes.add(funcDef());
            } else {
                top.varDefNodes.addAll(varDefs());
            }
        }
        return top;
    }

    /**
     * 判断是否是函数
     */
    private boolean isFunc() {
        Token type = lexer.nextIgnoreLineBreak();
        Token name = lexer.nextIgnoreLineBreak();
        Token label = lexer.peekIgnoreLineBreak();
        lexer.back(name);
        lexer.back(type);
        return label.type == Token.Type.OPEN_PARENTHESIS;
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
                    lexer.unexpectedToken(label.type, Token.Type.DOT);
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
        if (!isVarType(funcType.type) && funcType.type != Token.Type.VOID)
            lexer.error("func type can't be " + funcType.name);
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
        if (isArr()) {
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

            if (isLineEnd(label.type)) {
                lexer.nextIgnoreLineBreak();
                list.add(varDefNode);
                break;
            } else if (label.type == Token.Type.ASSIGN) {
                //TODO 解析赋值表达式
                varDefNode.value = expr();
            } else if (label.type == Token.Type.COMMA) {
                lexer.nextIgnoreLineBreak();
            } else {
                //遇到其他字符, 退出
                list.add(varDefNode);
                break;
            }
        }
        return list;
    }

    // 判断是否数组定义
    private boolean isArr() {
        Token type = lexer.nextIgnoreLineBreak();
        Token name = lexer.nextIgnoreLineBreak();
        Token label = lexer.peekIgnoreLineBreak();
        lexer.back(name);
        lexer.back(type);
        return label.type == Token.Type.L_SQUARE_BRACKET;
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
    private boolean isVarType(Token.Type type) {
        return type == Token.Type.IDENTIFIER
                || (type.ordinal() >= Token.Type.INT.ordinal() && type.ordinal() <= Token.Type.DOUBLE.ordinal());
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
            if (!isVarType(funcParamNode.paramType.type)) {
                lexer.error("func param type can't be " + funcParamNode.paramType.type.name());
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
                lexer.unexpectedToken(label.type);
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
        switch (lexer.peekIgnoreLineBreak().type) {
            case IF:
                return ifStmt();
            case WHILE:
                return whileStmt();
            case SWITCH:
                return switchStmtNode();
            case BREAK:

            default:
                return null;
        }
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
                lexer.unexpectedToken(token.type);
            }
            token = lexer.peekIgnoreLineBreak();
        }
        return switchStmt;
    }


    private ExprNode expr() {
        ExprNode cond = condExpr();
        Token label = lexer.peekIgnoreLineBreak();
        if (label.type.ordinal() >= Token.Type.ASSIGN.ordinal()
                && label.type.ordinal() <= Token.Type.RSH_ASSIGN.ordinal()) {
            lexer.nextIgnoreLineBreak();
            ExprNode assignExpr = expr();
        }
        return cond;
    }

    private ExprNode condExpr() {

    }

    private ExprNode factorExpr() {
        // 读取的token列表，函数结束时要back回去
        List<Token> tokens = new LinkedList<>();
        Token token = lexer.nextIgnoreLineBreak();
        if (token.type == Token.Type.NUMBER_VAL
                || token.type == Token.Type.STRING_VAL
                || token.type == Token.Type.DOUBLE_VAL
                || token.type == Token.Type.FLOAT_VAL
                || token.type == Token.Type.CHAR_VAL) {

        }
    }
}