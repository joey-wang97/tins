package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;

public class BitAndExpr extends ExprNode {
    public ExprNode leftExpr, rightExpr;

    public BitAndExpr(ExprNode leftExpr, ExprNode rightExpr) {
        this.leftExpr = leftExpr;
        this.rightExpr = rightExpr;
    }
}
