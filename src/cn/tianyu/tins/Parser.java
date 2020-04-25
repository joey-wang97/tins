package cn.tianyu.tins;

import cn.tianyu.tins.ast.*;
import cn.tianyu.tins.type.Token;

import java.util.ArrayList;
import java.util.List;

public class Parser {

    Lexer lexer;

    public Parser(String fileName) {
        lexer = new Lexer(fileName);
    }

    public TopDefNode parser() {
        TopDefNode top = new TopDefNode();
        Token token = lexer.peek();
        while (token.type != Token.Type.END) {
            if (token.type == Token.Type.IMPORT) {
                top.importNodes.add(importStmt());
            } else if (token.type == Token.Type.STRUCT) {
                top.structDefNodes.add(structDef());
            } else {
                token = lexer.next();
                Token id = lexer.match(Token.Type.IDENTIFIER);
                Token third = lexer.peek();
                lexer.back(id);
                lexer.back(token);
                // 根据第三个token是不是左括号进入函数定义
                if (third.type == Token.Type.LPARENT) {
                    top.funcDefNodes.add(funcDef());
                } else {
                    top.varDefNodes.addAll(varDefs());
                }
            }
            token = lexer.peek();
        }
        return top;
    }

    private boolean endOfLine(Token.Type type) {
        return type == Token.Type.NEWLINE || type == Token.Type.END;
    }

    private ImportNode importStmt() {
        ImportNode importNode = new ImportNode();
        lexer.match(Token.Type.IMPORT);
        while (true) {
            Token id = lexer.match(Token.Type.IDENTIFIER);
            importNode.folders.add(id.name);
            Token label = lexer.next();
            if (endOfLine(label.type))
                break;
            else if (label.type != Token.Type.DOT)
                lexer.unexpectedToken(label.type, Token.Type.DOT);
        }
        return importNode;
    }

    private StructDefNode structDef() {
        StructDefNode structDefNode = new StructDefNode();
        lexer.match(Token.Type.STRUCT);
        Token name = lexer.match(Token.Type.IDENTIFIER);
        lexer.match(Token.Type.LBRACE);
        Token token = lexer.next();
        Token id = lexer.match(Token.Type.IDENTIFIER);
        Token third = lexer.peek();
        lexer.back(id);
        lexer.back(token);
        // 根据第三个token是不是左括号进入函数定义
        if (third.type == Token.Type.LPARENT) {
            structDefNode.funcDefNodes.add(funcDef());
        } else {
            structDefNode.varDefNodes.addAll(varDefs());
        }
        lexer.match(Token.Type.RBRACE);
        return structDefNode;
    }

    private FuncDefNode funcDef() {
        FuncDefNode funcDefNode = new FuncDefNode();
        funcDefNode.funcType = lexer.next();
        funcDefNode.name = lexer.match(Token.Type.IDENTIFIER).name;
        funcDefNode.paramNode = funParam();
        funcDefNode.stmts = funcBody();
        return funcDefNode;
    }

    /**
     * 一行代码可能定义多个变量
     */
    private List<VarDefNode> varDefs() {
        List<VarDefNode> list = new ArrayList<>();
        Token varType = lexer.next();
        // 遇到左括号，为数组，将左右括号都next掉
        boolean isArr=false;
        if (lexer.peek().type == Token.Type.LBRACKET) {
            lexer.next();
            lexer.match(Token.Type.RBRACKET);
            isArr = true;
        }
        while (true) {
            VarDefNode varDefNode = new VarDefNode();
            varDefNode.varType = varType;
            varDefNode.isArr = isArr;
            varDefNode.name = lexer.match(Token.Type.IDENTIFIER).name;
            Token label = lexer.next();
            if (label.type == Token.Type.NEWLINE) {
                break;
            } else if (label.type == Token.Type.COMMA) {
                continue;
            } else if (label.type == Token.Type.ASSIGN) {
                //TODO 解析赋值表达式
                varDefNode.value = expr(isArr);
            }
        }
        return list;
    }

    // TODO
    private Object expr(boolean isArr) {
        Token token = lexer.next();
        while (token.type != Token.Type.COMMA) {
            token = lexer.next();
        }
        return null;
    }

    /**
     * 函数参数解析
     */
    private List<FuncParamNode> funParam() {
        lexer.match(Token.Type.LPARENT);
        List<FuncParamNode> nodes = new ArrayList<>();
        Token token = lexer.peek();
        while (token.type != Token.Type.RPARENT) {
            FuncParamNode funcParamNode = new FuncParamNode();
            Token paramType = lexer.next();
            String paramName = lexer.match(Token.Type.IDENTIFIER).name;
            Token label = lexer.peek();
            // 如果第三个token为逗号，则跳过逗号，并继续读取
            if (label.type == Token.Type.COMMA)
                lexer.next();
            nodes.add(funcParamNode);
        }
        lexer.match(Token.Type.RPARENT);
        return nodes;
    }

    private List<StmtNode> funcBody() {
        lexer.match(Token.Type.LBRACE);
        List<StmtNode> stmtNodes = new ArrayList<>();
        while (true) {
            Token token = lexer.peek();
            if (token.type == Token.Type.RBRACE)
                break;
            stmtNodes.add(stmt());
        }
        lexer.match(Token.Type.RBRACE);
        return stmtNodes;
    }

    private StmtNode stmt() {
        Token token = lexer.next();
        while (token.type != Token.Type.NEWLINE)
            token = lexer.next();
        return null;
    }
}
