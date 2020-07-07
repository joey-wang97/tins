package cn.tianyu.tins.ast.stmt;

import cn.tianyu.tins.ast.ExprNode;
import cn.tianyu.tins.ast.Node;
import cn.tianyu.tins.ast.StmtNode;

/**
 * 顶层结点
 */
public class ExprStmtNode extends StmtNode {

    public ExprNode expr;

    public ExprStmtNode(ExprNode expr) {
        this.expr = expr;
    }

    @Override
    public void dump(int indent) {
        dump(indent, "just expr:");
        expr.dump(indent+Node.INDENT);
    }
}
