package cn.tianyu.tins.ast;

import cn.tianyu.tins.type.Token;

/**
 * 顶层结点
 */
public class FuncParamNode extends Node{
    Token paramType;
    String name;

    @Override
    public void print(int indent) {
        print(indent, "FuncParam type: "+paramType.type.name());
        print(indent, "FuncParam type name: "+paramType.name);
        print(indent, "FuncParam name: "+name);
    }
}
