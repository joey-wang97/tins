package wang.joye.tins.ast.stmt;

import wang.joye.tins.ast.node.Node;
import wang.joye.tins.ast.node.StmtNode;
import wang.joye.tins.ast.node.VarDefNode;

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
    public void dump(int level) {
        dump(level, "var defs:");
        varDefNodes.forEach(i -> i.dump(level + Node.INDENT));
    }
}
