package cn.tianyu.tins.ast;

import java.util.ArrayList;
import java.util.List;

/**
 * 结构体
 */
public class StructDefNode extends Node {
    public String name;
    public List<VarDefNode> varDefNodes = new ArrayList<>();
    public List<FuncDefNode> funcDefNodes = new ArrayList<>();

    @Override
    public void dump(int indent) {
        dump(indent, "struct name: " + name);
        dump(indent, "struct var def:");
        varDefNodes.forEach(i -> i.dump(indent + Node.INDENT));
        dump(indent, "struct func def:");
        funcDefNodes.forEach(i -> i.dump(indent + Node.INDENT));
        System.out.println();
    }
}
