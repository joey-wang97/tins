package wang.joye.tins.ast.stmt;

import wang.joye.tins.ast.node.StmtNode;
import wang.joye.tins.util.DumpUtil;
import wang.joye.tins.visitor.ASTVisitor;

/**
 * 顶层结点
 */
public class EmptyStmtNode extends StmtNode {

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, "EMPTY");
    }

    @Override
    public void check(ASTVisitor visitor) {
        visitor.visit(this);
    }
}
