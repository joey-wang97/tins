package cn.tianyu.tins.ast;

import cn.tianyu.tins.type.Token;

/**
 * 顶层结点
 */
public class VarDefNode extends Node {

    public Token varType;
    public String varName;
    public boolean isArr;
    public Object value;

    @Override
    public void dump(int indent) {
        String varTypeName = varType.name == null ? varType.type.name() : varType.name;
        dump(indent, "var " + varTypeName +" " + varName + (isArr ? "[]" : ""));
        dump(indent, "value:" + value);
    }
}
