package cn.tianyu.tins.ast;

import cn.tianyu.tins.type.Token;

import java.util.List;

/**
 * 顶层结点
 */
public class FuncDefNode extends Node {
    public Token funcType;
    public String funcName;
    public List<FuncParamNode> paramNode;
    public List<StmtNode> stmts;

    public void dump(int indent) {
        String funcTypeName = funcType.name == null ? funcType.type.name() : funcType.name;

        dump(indent, "func " + funcTypeName + " " + funcName);
        dump(indent, "func params:");
        paramNode.forEach(i -> i.dump(indent + Node.INDENT));
        dump(indent, "func stmts:");
        stmts.forEach(i -> i.dump(indent + Node.INDENT));
    }
}
