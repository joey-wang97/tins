package cn.tianyu.tins;

import cn.tianyu.tins.ast.ImportNode;
import cn.tianyu.tins.symbol.FuncSymbol;
import cn.tianyu.tins.symbol.StructSymbol;
import cn.tianyu.tins.symbol.SymbolTable;
import cn.tianyu.tins.symbol.VarSymbol;
import cn.tianyu.tins.type.Token;

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
        skipFuncBody();
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
            // 一次只能定义一个数组
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

    private void skipFuncBody() {
        lexer.matchIgnoreLineBreak(Token.Type.L_CURLY_BRACKET);
        while (lexer.next().type != Token.Type.R_CURLY_BRACKET)
            ;
    }

    //表达式，默认为赋值表达式
    private void skipExpr() {
        skipCondExpr();
        Token label = lexer.peek();
        if (label.type.ordinal() >= Token.Type.ASSIGN.ordinal()
                && label.type.ordinal() <= Token.Type.RSH_ASSIGN.ordinal()) {
            lexer.next();
            skipExpr();
        }
    }

    private void skipCondExpr() {
        skipLogicOrExpr();
        Token label = lexer.peek();
        if (label.type == Token.Type.QUESTION_MARK) {
            lexer.next();
            skipExpr();
            lexer.matchIgnoreLineBreak(Token.Type.COLON);
            skipCondExpr();
        }
    }

    // 这里使用递归代替while循环
    private void skipLogicOrExpr() {
        skipLogicAndExpr();
        if (lexer.peek().type == Token.Type.OR) {
            lexer.next();
            skipLogicOrExpr();
        }
    }

    private void skipLogicAndExpr() {
        skipBitOrExpr();
        while (lexer.peek().type == Token.Type.AND) {
            lexer.next();
            skipBitOrExpr();
        }
    }

    private void skipBitOrExpr() {
        skipBitXorExpr();
        while (lexer.peek().type == Token.Type.B_OR) {
            lexer.next();
            skipBitXorExpr();
        }
    }

    private void skipBitXorExpr() {
        skipBitAndExpr();
        while (lexer.peek().type == Token.Type.B_XOR) {
            lexer.next();
            skipBitAndExpr();
        }
    }

    private void skipBitAndExpr() {
        skipEqualityExpr();
        while (lexer.peek().type == Token.Type.B_AND) {
            lexer.next();
            skipEqualityExpr();
        }
    }

    private void skipEqualityExpr() {
        skipRelationExpr();
        while (lexer.peek().type == Token.Type.EQ
                || lexer.peek().type == Token.Type.NE) {
            lexer.nextIgnoreLineBreak();
            skipRelationExpr();
        }
    }

    private void skipRelationExpr() {
        skipShiftExpr();
        while (lexer.peek().type.ordinal() >= Token.Type.GT.ordinal()
                && lexer.peek().type.ordinal() <= Token.Type.LT.ordinal()) {
            lexer.next();
            skipShiftExpr();
        }
    }

    private void skipShiftExpr() {
        skipAddOrSubExpr();
        while (lexer.peek().type == Token.Type.LSH
                || lexer.peek().type == Token.Type.RSH) {
            lexer.next();
            skipAddOrSubExpr();
        }
    }

    private void skipAddOrSubExpr() {
        skipMulOrDivExpr();
        while (lexer.peek().type == Token.Type.ADD
                || lexer.peek().type == Token.Type.SUB) {
            lexer.next();
            skipMulOrDivExpr();
        }
    }

    private void skipMulOrDivExpr() {
        skipSuffixUnaryExpr();
        while (lexer.peek().type == Token.Type.MUL
                || lexer.peek().type == Token.Type.DIV) {
            lexer.next();
            skipSuffixUnaryExpr();
        }
    }

    private void skipSuffixUnaryExpr() {
        skipPrefixUnaryExpr();
        if (lexer.peek().type == Token.Type.INC
                || lexer.peek().type == Token.Type.DEC) {
            lexer.next();
        }
    }

    private void skipPrefixUnaryExpr() {
        Token.Type labelType = lexer.peekIgnoreLineBreak().type;
        // 取反，取非，都是可递归的
        if (labelType == Token.Type.TILDE || labelType == Token.Type.NOT) {
            lexer.nextIgnoreLineBreak();
            skipPrefixUnaryExpr();
        } else if (labelType == Token.Type.INC // 这些操作符不可递归
                || labelType == Token.Type.DEC
                || labelType == Token.Type.SUB) {
            lexer.nextIgnoreLineBreak();
            skipFactorExpr();
        }
        skipFactorExpr();
    }

    private void skipFactorExpr() {
        Token token = lexer.nextIgnoreLineBreak();
        if (token.type == Token.Type.NUMBER_VAL
                || token.type == Token.Type.STRING_VAL
                || token.type == Token.Type.DOUBLE_VAL
                || token.type == Token.Type.FLOAT_VAL
                || token.type == Token.Type.CHAR_VAL) {
        }
    }
}