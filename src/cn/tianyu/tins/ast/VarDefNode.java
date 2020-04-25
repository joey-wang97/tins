package cn.tianyu.tins.ast;

import cn.tianyu.tins.type.Token;

/**
 * 顶层结点
 */
public class VarDefNode extends Node{

    public Token varType;
    public String name;
    public boolean isArr;
    public Object value;
}
