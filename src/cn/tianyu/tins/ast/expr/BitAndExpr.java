package cn.tianyu.tins.ast.expr;

import cn.tianyu.tins.ast.ExprNode;

public class BitAndExpr extends ExprNode {
    public ExprNode leftExpr, rightExpr;

    public BitAndExpr(ExprNode leftExpr, ExprNode rightExpr) {
        this.leftExpr = leftExpr;
        this.rightExpr = rightExpr;
    }
}
