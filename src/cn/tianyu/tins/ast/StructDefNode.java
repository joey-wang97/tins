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
    public void print(int indent) {
        print(indent, "struct name: " + name);
        print(indent, "struct var def:");
        varDefNodes.forEach(i -> i.print(indent + super.indent));
        print(indent, "struct func def:");
        funcDefNodes.forEach(i -> i.print(indent + super.indent));
    }
}
