package cn.tianyu.tins.ast;

import cn.tianyu.tins.type.Token;

/**
 * 顶层结点
 */
public class FuncParamNode extends Node {
    public Token paramType;
    public String paramName;
    public boolean isArr;

    @Override
    public void dump(int indent) {
        String paramTypeName = paramType.name == null ? paramType.type.name() : paramType.name;

        dump(indent, "FuncParam: " + paramTypeName + " " + paramName + (isArr ? "[]" : ""));
    }
}
