package cn.tianyu.tins;

import cn.tianyu.tins.ast.TopDefNode;
import cn.tianyu.tins.symbol.SymbolTable;
import cn.tianyu.tins.type.Token;
import com.alibaba.fastjson.JSON;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class Main {

    static Main main = new Main();

    public static void main(String[] args) {
        String testFile = "sample/test1.tins";
        // testLexer(testFile);
        testParser(testFile);
        // testPreParser(testFile);
    }

    public static void testLexer(String fileName) {
        Lexer lexer = new Lexer(fileName);
        System.out.println(lexer.peek().type.name());
        System.out.println(lexer.lookAhead(1).type.name());
        Token token = lexer.next();
        System.out.println(lexer.peek().type.name());
        System.out.println(lexer.lookAhead(1).type.name());
    }

    public static void testParser(String fileName) {
        Parser parser = new Parser(fileName);
        TopDefNode ast = parser.parser();
        writeToFile("test.json", JSON.toJSONString(ast));
        // ast.dump();
    }

    public static int get1() {
        return a;
    }

    static int a = get1();

    public static void testPreParser(String fileName) {
        Lexer lexer = new Lexer(fileName);
        DefParser defParser = new DefParser(lexer);
        defParser.preParser();
        // 输出符号表
        SymbolTable.dumpSymbol();
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
