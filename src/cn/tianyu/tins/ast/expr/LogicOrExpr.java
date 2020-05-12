package cn.tianyu.tins.ast.expr;

import cn.tianyu.tins.ast.ExprNode;

public class LogicOrExpr extends ExprNode {
    public ExprNode leftExpr, rightExpr;

    public LogicOrExpr(ExprNode leftExpr, ExprNode rightExpr) {
        this.leftExpr = leftExpr;
        this.rightExpr = rightExpr;
    }
}
