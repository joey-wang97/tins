package cn.tianyu.tins.ast.expr;

import cn.tianyu.tins.ast.ExprNode;

public class BitOrExpr extends ExprNode {
    public ExprNode leftExpr, rightExpr;

    public BitOrExpr(ExprNode leftExpr, ExprNode rightExpr) {
        this.leftExpr = leftExpr;
        this.rightExpr = rightExpr;
    }
}
