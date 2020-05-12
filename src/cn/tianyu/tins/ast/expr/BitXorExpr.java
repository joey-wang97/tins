package cn.tianyu.tins.ast.expr;

import cn.tianyu.tins.ast.ExprNode;

public class BitXorExpr extends ExprNode {
    public ExprNode leftExpr, rightExpr;

    public BitXorExpr(ExprNode leftExpr, ExprNode rightExpr) {
        this.leftExpr = leftExpr;
        this.rightExpr = rightExpr;
    }
}
