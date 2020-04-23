package cn.tianyu.tins;

import cn.tianyu.tins.ast.TopDefNode;

public class Main {

    public static void main(String[] args) {
        Parser parser = new Parser("test1.tins");
        TopDefNode ast = parser.parser();
        ast.print();
    }
}
