package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;

public class BitOrExpr extends ExprNode {
    public ExprNode leftExpr, rightExpr;

    public BitOrExpr(ExprNode leftExpr, ExprNode rightExpr) {
        this.leftExpr = leftExpr;
        this.rightExpr = rightExpr;
    }
}
