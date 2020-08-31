package wang.joye.tins.ast.stmt;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.ast.node.StmtNode;
import wang.joye.tins.util.DumpUtil;
import wang.joye.tins.visitor.ASTVisitor;

import java.util.LinkedList;
import java.util.List;

/**
 * 顶层结点
 */
public class IfStmtNode extends StmtNode {

    public ExprNode condition;
    public StmtNode ifStmt;
    // else if语句
    public List<ElseIfStmt> elseIfStmts = new LinkedList<>();
    public StmtNode elseStmt;

    public static class ElseIfStmt {
        public ExprNode condition;
        public StmtNode stmt;
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, "IfStmtNode:");
        DumpUtil.dump(level + 1, "condition:");
        condition.dump(level + 2);
        DumpUtil.dump(level + 1, "if stmt:");
        ifStmt.dump(level + 2);
        elseIfStmts.forEach(i -> {
            DumpUtil.dump(level + 1, "else if stmt:");
            DumpUtil.dump(level + 2, "condition:");
            i.condition.dump(level + 3);
            DumpUtil.dump(level + 2, "stmt:");
            i.stmt.dump(level + 3);
        });
        if (elseStmt != null) {
            DumpUtil.dump(level + 1, "else stmt:");
            elseStmt.dump(level + 2);
        }
    }

    @Override
    public void check(ASTVisitor visitor) {
        visitor.visit(this);
    }
}
