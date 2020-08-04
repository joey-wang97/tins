package wang.joye.tins.ast.node.stmt;

import wang.joye.tins.ast.node.StmtNode;
import wang.joye.tins.ast.node.VarDefNode;
import wang.joye.tins.util.DumpUtil;

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
        DumpUtil.dump(level, "var defs:");
        varDefNodes.forEach(i -> i.dump(level + 1));
    }
}
