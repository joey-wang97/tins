package cn.tianyu.tins;

import cn.tianyu.tins.ast.*;
import cn.tianyu.tins.type.Token;

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
                    top.varDefNodes.add(varDef());
                }
            }
            token = lexer.next();
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
        return structDefNode;
    }

    private FuncDefNode funcDef() {
        FuncDefNode funcDefNode = new FuncDefNode();
        Token type = lexer.next();
        Token name = lexer.next();
        lexer.match(Token.Type.LPARENT);
        funParamDef();
        return funcDefNode;
    }

    private VarDefNode varDef() {
        VarDefNode varDefNode = new VarDefNode();
        return varDefNode;
    }

    /**
     * 函数参数解析
     */
    private FuncParamNode funParamDef() {
        FuncParamNode funcParamNode = new FuncParamNode();
        Token token = lexer.peek();
        if (token.type == Token.Type.RPARENT)
            return funcParamNode;
        while (true) {
            Token paramType = lexer.next();
            Token paramName = lexer.match(Token.Type.IDENTIFIER);
            // 第三个token，只能为逗号或右括号
            Token label = lexer.next();
            if (label.type == Token.Type.RPARENT)
                break;
            else if (label.type != Token.Type.COMMA)
                lexer.unexpectedToken(label.type);
        }
        return funcParamNode;
    }
}
