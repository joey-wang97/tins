package wang.joye.tins.ast.stmt;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.ast.node.StmtNode;
import wang.joye.tins.visitor.ASTVisitor;

/**
 * 顶层结点
 */
public class CaseStmtNode extends StmtNode {

    public ExprNode condition;
    public StmtNode stmt;
    public boolean isBreak;

    @Override
    public void dump(int level) {

    }

    @Override
    public void check(ASTVisitor visitor) {
        visitor.visit(this);
    }
}
