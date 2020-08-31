package wang.joye.tins.ast.stmt;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.ast.node.StmtNode;
import wang.joye.tins.util.DumpUtil;
import wang.joye.tins.visitor.ASTVisitor;

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
        DumpUtil.dump(level, "ExprStmt");
        expr.dump(level +1);
    }

    @Override
    public void check(ASTVisitor visitor) {
        visitor.visit(this);
    }
}
