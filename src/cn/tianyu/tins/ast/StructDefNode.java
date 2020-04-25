package cn.tianyu.tins.ast;

import java.util.List;

/**
 * 结构体
 */
public class StructDefNode extends Node{
    public String name;
    public List<VarDefNode> varDefNodes;
    public List<FuncDefNode> funcDefNodes;
}
