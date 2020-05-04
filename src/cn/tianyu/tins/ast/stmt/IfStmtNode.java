package cn.tianyu.tins.ast.stmt;

import cn.tianyu.tins.ast.ExprNode;
import cn.tianyu.tins.ast.Node;
import cn.tianyu.tins.ast.StmtNode;

import java.util.List;

/**
 * 顶层结点
 */
public class IfStmtNode extends StmtNode {

    public ExprNode condition;
    public StmtNode ifStmt;
    public StmtNode elseStmt;

    @Override
    public void dump(int indent) {
        super.printIndent(indent);
        System.out.println();
    }
}
