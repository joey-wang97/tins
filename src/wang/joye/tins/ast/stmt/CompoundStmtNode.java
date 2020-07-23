package wang.joye.tins.ast.stmt;

import wang.joye.tins.ast.node.StmtNode;

import java.util.List;

/**
 * 复合语句
 */
public class CompoundStmtNode extends StmtNode {

    public List<StmtNode> stmts;

    @Override
    public void dump(int level) {
    }
}
