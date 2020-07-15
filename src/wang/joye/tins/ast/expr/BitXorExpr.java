package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;

public class BitXorExpr extends ExprNode {
    public ExprNode leftExpr, rightExpr;

    public BitXorExpr(ExprNode leftExpr, ExprNode rightExpr) {
        this.leftExpr = leftExpr;
        this.rightExpr = rightExpr;
    }
}
