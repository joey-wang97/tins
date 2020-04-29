package cn.tianyu.tins.ast;

import java.util.ArrayList;
import java.util.List;

/**
 * 顶层结点
 */
public class TopDefNode extends Node {
    public List<ImportNode> importNodes = new ArrayList<>();
    public List<StructDefNode> structDefNodes = new ArrayList<>();
    public List<VarDefNode> varDefNodes = new ArrayList<>();
    public List<FuncDefNode> funcDefNodes = new ArrayList<>();

    public void print() {
        System.out.println("import list");
        importNodes.forEach(i -> i.print(indent));
        System.out.println("struct def list");
        structDefNodes.forEach(i -> i.print(indent));
        System.out.println("var def list");
        varDefNodes.forEach(i -> i.print(indent));
        System.out.println("func def list");
        funcDefNodes.forEach(i -> i.print(indent));
    }
}
