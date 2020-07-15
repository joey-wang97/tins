package wang.joye.tins.ast.stmt;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.ast.node.Node;
import wang.joye.tins.ast.node.StmtNode;

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
        dump(level, "just expr:");
        expr.dump(level +Node.INDENT);
    }
}
