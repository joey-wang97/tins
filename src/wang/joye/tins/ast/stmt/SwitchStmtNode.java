package wang.joye.tins.ast.stmt;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.ast.node.StmtNode;

import java.util.List;

/**
 * 顶层结点
 */
public class SwitchStmtNode extends StmtNode {

    public ExprNode condition;
    public List<CaseStmtNode> caseStmts;
    public StmtNode defaultStmt;

    @Override
    public void dump(int level) {
        super.printIndent(level);
        System.out.println();
    }
}
