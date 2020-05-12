package cn.tianyu.tins.ast.expr;

import cn.tianyu.tins.ast.ExprNode;

public class LogicAndExpr extends ExprNode {
    public ExprNode leftExpr, rightExpr;

    public LogicAndExpr(ExprNode leftExpr, ExprNode rightExpr) {
        this.leftExpr = leftExpr;
        this.rightExpr = rightExpr;
    }
}
