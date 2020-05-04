package cn.tianyu.tins.ast;

import java.util.Arrays;

public abstract class Node {
    public int type;
    public static int INDENT = 4;

    public enum NodeType {

    }

    public abstract void dump(int indent);

    public void printIndent(int indent) {
        char[] indents = new char[indent];
        Arrays.fill(indents, '-');
        System.out.print(indents);
    }

    public void dump(int indent, String str) {
        printIndent(indent);
        System.out.println(str);
    }
}
