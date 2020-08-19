package wang.joye.tins.ast.stmt;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.ast.node.StmtNode;
import wang.joye.tins.util.DumpUtil;

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
        DumpUtil.dump(level);
        System.out.println();
    }
}
