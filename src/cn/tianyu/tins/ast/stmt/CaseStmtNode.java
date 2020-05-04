package cn.tianyu.tins.ast.stmt;

import cn.tianyu.tins.ast.ExprNode;
import cn.tianyu.tins.ast.StmtNode;

/**
 * 顶层结点
 */
public class CaseStmtNode extends StmtNode {

    public ExprNode condition;
    public StmtNode stmt;
    public boolean isBreak;

    @Override
    public void dump(int indent) {
        super.printIndent(indent);
        System.out.println();
    }
}
