package cn.tianyu.tins.ast;

import cn.tianyu.tins.type.Token;

/**
 * 顶层结点
 */
public class VarDefNode extends Node {

    public Token varType;
    public String name;
    public boolean isArr;
    public Object value;

    @Override
    public void print(int indent) {
        super.print(indent);
        System.out.println("var type:" + varType.type.name());
        super.print(indent);
        System.out.println("var type name:" + varType.name);
        print(indent, "var name: " + name);
        print(indent, "isArr:" + isArr);
        print(indent, "value:" + value);
    }
}
