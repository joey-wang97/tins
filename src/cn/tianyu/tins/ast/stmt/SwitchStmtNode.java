package cn.tianyu.tins.ast.stmt;

import cn.tianyu.tins.ast.ExprNode;
import cn.tianyu.tins.ast.StmtNode;

import java.util.List;

/**
 * 顶层结点
 */
public class SwitchStmtNode extends StmtNode {

    public ExprNode condition;
    public List<CaseStmtNode> caseStmts;
    public StmtNode defaultStmt;

    @Override
    public void dump(int indent) {
        super.printIndent(indent);
        System.out.println();
    }
}
