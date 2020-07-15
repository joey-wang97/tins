package wang.joye.tins;

import wang.joye.tins.ast.node.ImportNode;
import wang.joye.tins.symbol.FuncSymbol;
import wang.joye.tins.symbol.StructSymbol;
import wang.joye.tins.symbol.SymbolTable;
import wang.joye.tins.symbol.VarSymbol;
import wang.joye.tins.type.Token;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

/**
 * 只读取定义和符号，不解析语句
 */
public class DefParser {

    Lexer lexer;

    public DefParser(Lexer lexer) {
        this.lexer = lexer;
    }

    public void preParser() {

        // 先一次性加载所有import语句
        // todo 解析import中的符号
        if (lexer.peekIgnoreLineBreak().type == Token.Type.IMPORT) {
            importStmts();
        }

        while (lexer.peekIgnoreLineBreak().type != Token.Type.END) {
            if (lexer.peekIgnoreLineBreak().type == Token.Type.STRUCT) {
                SymbolTable.structSymbols.add(structDef());
            } else if (lexer.peekIgnoreLineBreak().type == Token.Type.FUNC) {
                SymbolTable.funcSymbols.add(funcDef());
            } else {
                SymbolTable.varSymbols.addAll(varDefs());
            }
        }
        SymbolTable.dumpSymbol();
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

    private StructSymbol structDef() {
        StructSymbol structSymbol = new StructSymbol();
        lexer.matchIgnoreLineBreak(Token.Type.STRUCT);
        structSymbol.name = lexer.matchIgnoreLineBreak(Token.Type.IDENTIFIER).name;
        lexer.matchIgnoreLineBreak(Token.Type.L_CURLY_BRACKET);

        while (lexer.peekIgnoreLineBreak().type != Token.Type.R_CURLY_BRACKET) {
            if (lexer.peekIgnoreLineBreak().type == Token.Type.FUNC) {
                funcDef();
            } else {
                structSymbol.fieldSymbols.addAll(varDefs());
            }
        }
        lexer.matchIgnoreLineBreak(Token.Type.R_CURLY_BRACKET);
        return structSymbol;
    }

    private FuncSymbol funcDef() {
        FuncSymbol funcDefNode = new FuncSymbol();
        lexer.matchIgnoreLineBreak(Token.Type.FUNC);
        Token funcType = lexer.nextIgnoreLineBreak();
        // 检查函数类型是否合法
        if (!isVarType(funcType.type) && funcType.type != Token.Type.VOID)
            lexer.error(funcType, "func type can't be " + funcType.name);
        funcDefNode.funcType = funcType;
        funcDefNode.funcName = lexer.matchIgnoreLineBreak(Token.Type.IDENTIFIER).name;
        funcDefNode.params = funParam();
        skipCompoundStmt();
        return funcDefNode;
    }

    int a = 1;

    /**
     * 一行代码可能定义多个变量
     */
    private List<VarSymbol> varDefs() {
        List<VarSymbol> list = new ArrayList<>();

        // 数组定义
        if (lexer.lookAheadIgnoreLineBreak(2).type == Token.Type.L_SQUARE_BRACKET) {
            // 一行只能定义一个数组
            list.add(arrDef());
            return list;
        }

        Token varType = lexer.nextIgnoreLineBreak();
        while (true) {
            VarSymbol varSymbol = new VarSymbol();
            varSymbol.varType = varType;
            varSymbol.isArr = false;
            varSymbol.varName = lexer.match(Token.Type.IDENTIFIER).name;
            // 检查后面的符号，不可换行
            Token label = lexer.peek();
            if (label.type == Token.Type.ASSIGN) {
                lexer.next();
                skipExpr();
                label = lexer.peek();
            }

            list.add(varSymbol);
            if (isLineEnd(label.type)) {
                break;
            } else if (label.type == Token.Type.COMMA) {
                lexer.next();
            } else {
                lexer.unexpectedToken(label);
            }
        }
        return list;
    }
    /**
     * 一次只能定义一个数组
     */
    private VarSymbol arrDef() {
        VarSymbol varDef = new VarSymbol();
        varDef.isArr = true;
        varDef.varType = lexer.nextIgnoreLineBreak();
        varDef.varName = lexer.match(Token.Type.IDENTIFIER).name;
        lexer.match(Token.Type.L_SQUARE_BRACKET);
        while (lexer.next().type != Token.Type.R_SQUARE_BRACKET)
            ;
        if (lexer.peek().type == Token.Type.ASSIGN) {
            skipCompoundStmt();
        }
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
    private List<VarSymbol> funParam() {
        List<VarSymbol> nodes = new LinkedList<>();
        lexer.matchIgnoreLineBreak(Token.Type.OPEN_PARENTHESIS);
        // 空函数参数
        if (lexer.peekIgnoreLineBreak().type == Token.Type.CLOSE_PARENTHESIS) {
            lexer.nextIgnoreLineBreak();
            return nodes;
        }

        while (true) {
            VarSymbol funcParamNode = new VarSymbol();
            funcParamNode.varType = lexer.nextIgnoreLineBreak();
            if (!isVarType(funcParamNode.varType.type)) {
                lexer.error(funcParamNode.varType, "func param type can't be " + funcParamNode.varType.type.name());
            }
            funcParamNode.varName = lexer.matchIgnoreLineBreak(Token.Type.IDENTIFIER).name;
            Token label = lexer.nextIgnoreLineBreak();
            // 判断数组
            if (label.type == Token.Type.L_SQUARE_BRACKET) {
                funcParamNode.isArr = true;
                lexer.matchIgnoreLineBreak(Token.Type.R_SQUARE_BRACKET);
                label = lexer.nextIgnoreLineBreak();
            }

            nodes.add(funcParamNode);
            if (label.type == Token.Type.CLOSE_PARENTHESIS) {
                break;
            } else if (label.type != Token.Type.COMMA) {
                lexer.unexpectedToken(label);
            }
        }
        return nodes;
    }

    /**
     * 跳过花括号内的所有语句
     */
    private void skipCompoundStmt() {
        lexer.matchIgnoreLineBreak(Token.Type.L_CURLY_BRACKET);
        while (lexer.next().type != Token.Type.R_CURLY_BRACKET)
            ;
    }


    // 跳过一个表达式，单行表达式中可能用逗号分隔了多个表达式
    private void skipExpr() {
        Token token = lexer.next();
        while (token.type != Token.Type.COMMA
                && token.type != Token.Type.LINE_BREAK) {

            // 三目运算符，三目运算符分量中也可以包含各种括号，头疼..
            if (token.type == Token.Type.QUESTION_MARK) {
                // 匹配到冒号
                while (token.type != Token.Type.COLON) {
                    token = lexer.nextIgnoreLineBreak();
                }
            }
            // 如果遇到圆括号，则跳过其中的token
            while (token.type == Token.Type.OPEN_PARENTHESIS) {
                // 左括号数量
                int count = 1;
                while (count > 0) {
                    token = lexer.nextIgnoreLineBreak();
                    if (token.type == Token.Type.OPEN_PARENTHESIS)
                        count++;
                    else if (token.type == Token.Type.CLOSE_PARENTHESIS)
                        count--;
                }
                token = lexer.nextIgnoreLineBreak();
            }
            // 如果遇到方括号，则跳过其中的token
            while (token.type == Token.Type.L_SQUARE_BRACKET) {
                // 左括号数量
                int count = 1;
                while (count > 0) {
                    token = lexer.nextIgnoreLineBreak();
                    if (token.type == Token.Type.L_SQUARE_BRACKET)
                        count++;
                    else if (token.type == Token.Type.R_SQUARE_BRACKET)
                        count--;
                }
                token = lexer.nextIgnoreLineBreak();
            }
            // 如果遇到花括号，则跳过其中的token
            while (token.type == Token.Type.L_CURLY_BRACKET) {
                // 左括号数量
                int count = 1;
                while (count > 0) {
                    token = lexer.nextIgnoreLineBreak();
                    if (token.type == Token.Type.L_CURLY_BRACKET)
                        count++;
                    else if (token.type == Token.Type.R_CURLY_BRACKET)
                        count--;
                }
                token = lexer.nextIgnoreLineBreak();
            }
            // 如果下一个token是换行符，且当前token为强运算符，则读取下一个非换行符
            // 如 a + \n 2，当前token为+，应让token跳到2的位置
            boolean ignoreLineBreak = false;
            for (int i = 0; lexer.peek().type == Token.Type.LINE_BREAK
                    && i < Token.STRONG_OPERATOR.length; i++) {
                if (token.type == Token.STRONG_OPERATOR[i]) {
                    ignoreLineBreak = true;
                    break;
                }
            }
            // 此处不能直接next，因为next可能会读取逗号
            token = ignoreLineBreak ? lexer.peekIgnoreLineBreak() : lexer.peek();
            // 如果下一个不是逗号，则next掉
            if (token.type != Token.Type.COMMA)
                lexer.next();
        }
    }
}