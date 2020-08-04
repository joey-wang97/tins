package wang.joye.tins.ast.node.stmt;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.ast.node.StmtNode;
import wang.joye.tins.util.DumpUtil;

/**
 * 顶层结点
 */
public class ExprStmtNode extends StmtNode {

    public ExprNode expr;

    public ExprStmtNode(ExprNode expr) {
        this.expr = expr;
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, "just expr:");
        expr.dump(level +1);
    }
}
