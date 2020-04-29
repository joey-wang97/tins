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

    @Override
    public void print(int indent) {
        super.print(indent);
        System.out.println("func type:" + funcType.type.name());
        super.print(indent);
        System.out.println("func type name:" + funcType.name);
        print(indent, "func name: " + name);
        print(indent, "func params");
        paramNode.forEach(i->i.print(indent+super.indent));
        print(indent, "func stmts");
        stmts.forEach(i->i.print(indent+super.indent));
    }
}
