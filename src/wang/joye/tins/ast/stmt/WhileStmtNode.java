package wang.joye.tins.ast.stmt;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.ast.node.StmtNode;
import wang.joye.tins.util.DumpUtil;

/**
 * 顶层结点
 */
public class WhileStmtNode extends StmtNode {

    public ExprNode condition;
    public StmtNode stmt;

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, "While stmt");
        DumpUtil.dump(level + 1, "condition");
        condition.dump(level + 2);
        DumpUtil.dump(level + 1, "stmt");
        stmt.dump(level + 2);
    }
}
