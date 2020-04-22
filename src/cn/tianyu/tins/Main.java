package cn.tianyu.tins;

import cn.tianyu.tins.type.Token;

public class Main {

    public static void main(String[] args) {
        Lexer lexer = new Lexer("test.tins");
        Token token = lexer.next();
        while (token.type != Token.Type.END) {
            System.out.println(token.type.name());
            token = lexer.next();
        }
    }
}
