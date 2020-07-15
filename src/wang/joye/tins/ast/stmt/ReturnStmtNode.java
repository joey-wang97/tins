package wang.joye.tins.ast.stmt;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.ast.node.Node;
import wang.joye.tins.ast.node.StmtNode;

/**
 * 顶层结点
 */
public class ReturnStmtNode extends StmtNode {

    public ExprNode expr;

    public ReturnStmtNode(ExprNode expr) {
        this.expr = expr;
    }

    @Override
    public void dump(int level) {
        dump(level, "return");
        expr.dump(level +Node.INDENT);
    }
}
