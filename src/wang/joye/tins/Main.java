package wang.joye.tins;

import wang.joye.tins.ast.AST;
import wang.joye.tins.type.Token;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class Main {

    static Main main = new Main();

    public static void main(String[] args) {
        String testFile = "sample/simple1.tins";
        // testLexer(testFile);
        testParser(testFile);
        // testPreParser(testFile);
    }

    public static void testLexer(String fileName) {
        Lexer lexer = new Lexer(fileName);
        Token token = lexer.next();
        /*System.out.println(lexer.peek().type.name());
        System.out.println(lexer.lookAhead(1).type.name());
        System.out.println(lexer.peek().type.name());
        System.out.println(lexer.lookAhead(1).type.name());*/
        while (token.type != Token.Type.END) {
            System.out.println(token.toString());
            token = lexer.next();
        }
    }

    public static void testParser(String fileName) {
        Parser parser = new Parser(fileName);
        AST ast = parser.parser();
        // writeToFile("test.json", JSON.toJSONString(ast));
        ast.dump();
    }

    public static void writeToFile(String filePath, String content) {
        try {
            BufferedWriter writer = new BufferedWriter(new FileWriter(filePath));
            writer.write(content);
            writer.flush();
            writer.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
