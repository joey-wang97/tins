package cn.tianyu.tins.ast;

import java.util.Arrays;

public class Node {
    public int type;
    public int indent = 4;

    public enum NodeType {

    }

    public void print(int indent) {
        char[] indents = new char[indent];
        Arrays.fill(indents, '-');
        System.out.print(indents);
    }

    public void print(int indent, String str) {
        char[] indents = new char[indent];
        Arrays.fill(indents, '-');
        System.out.print(indents);
        System.out.println(str);
    }
}
