package wang.joye.tins.ast.stmt;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.ast.node.StmtNode;
import wang.joye.tins.util.DumpUtil;
import wang.joye.tins.visitor.ASTVisitor;

/**
 * 顶层结点
 */
public class ForStmtNode extends StmtNode {

    public StmtNode init;
    public ExprNode condition, operation;
    public StmtNode stmt;

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, this);
    }
    @Override
    public void check(ASTVisitor visitor) {
        visitor.visit(this);
    }
}
