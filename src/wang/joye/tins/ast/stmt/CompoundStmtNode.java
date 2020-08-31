package wang.joye.tins.ast.stmt;

import wang.joye.tins.ast.node.StmtNode;
import wang.joye.tins.util.DumpUtil;
import wang.joye.tins.visitor.ASTVisitor;

import java.util.LinkedList;
import java.util.List;

/**
 * 复合语句
 */
public class CompoundStmtNode extends StmtNode {

    public List<StmtNode> stmts = new LinkedList<>();

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, "compound stmts:");
        stmts.forEach(i->i.dump(level+1));
    }

    @Override
    public void check(ASTVisitor visitor) {
        visitor.visit(this);
    }
}
