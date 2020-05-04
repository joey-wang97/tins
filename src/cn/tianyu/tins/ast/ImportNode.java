package cn.tianyu.tins.ast;

import java.util.LinkedList;
import java.util.List;

/**
 * 顶层结点
 */
public class ImportNode extends Node {

    public List<String> folders = new LinkedList<>();

    public void dump(int indent) {
        printIndent(indent);
        folders.forEach(i -> System.out.print(i + " "));
        System.out.println();
    }
}
