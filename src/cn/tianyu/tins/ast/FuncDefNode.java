package cn.tianyu.tins.ast;

import cn.tianyu.tins.type.Token;

import java.util.List;

/**
 * 顶层结点
 */
public class FuncDefNode extends Node{
    public Token funcType;
    public String name;
    public List<FuncParamNode> paramNode;
    public List<StmtNode> stmts;
}
