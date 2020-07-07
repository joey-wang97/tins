package cn.tianyu.tins.ast.stmt;

import cn.tianyu.tins.ast.Node;
import cn.tianyu.tins.ast.StmtNode;
import cn.tianyu.tins.ast.VarDefNode;

import java.util.List;

/**
 * 函数内的变量定义语句
 */
public class VarDefStmtNode extends StmtNode {

    public List<VarDefNode> varDefNodes;

    public VarDefStmtNode(List<VarDefNode> varDefNodes) {
        this.varDefNodes = varDefNodes;
    }

    @Override
    public void dump(int indent) {
        dump(indent, "var defs:");
        varDefNodes.forEach(i -> i.dump(indent + Node.INDENT));
    }
}
