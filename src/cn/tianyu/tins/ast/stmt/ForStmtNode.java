package cn.tianyu.tins.ast.stmt;

import cn.tianyu.tins.ast.ExprNode;
import cn.tianyu.tins.ast.StmtNode;

/**
 * 顶层结点
 */
public class ForStmtNode extends StmtNode {

    public ExprNode init,condition, operation;
    public StmtNode stmt;

    @Override
    public void dump(int indent) {
        super.printIndent(indent);
        System.out.println();
    }
}
