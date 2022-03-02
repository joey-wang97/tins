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
        DumpUtil.dump(level, this);
    }

    @Override
    public void check(ASTVisitor visitor) {
        visitor.visit(this);
    }
}
