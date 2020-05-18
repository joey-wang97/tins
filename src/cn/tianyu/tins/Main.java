package cn.tianyu.tins;

import cn.tianyu.tins.ast.TopDefNode;
import cn.tianyu.tins.type.Token;

public class Main {

    public static void main(String[] args) {
        // testParser("sample/test1.tins");
        testLexer("sample/test1.tins");
    }

    public static void testLexer(String fileName) {
        Lexer lexer = new Lexer(fileName);
        Token token = lexer.next();
        while (token.type != Token.Type.END) {
            System.out.println(token.type.name());
            token = lexer.next();
        }
    }

    public static void testParser(String fileName) {
        Parser parser = new Parser(fileName);
        TopDefNode ast = parser.parser();
        // ast.dump();
    }

}
