package cn.tianyu.tins.ast.expr;

import cn.tianyu.tins.ast.ExprNode;
import cn.tianyu.tins.type.Token;

public class CondExpr extends ExprNode {
    public ExprNode cond, trueExpr, falseExpr;

    public CondExpr(ExprNode cond, ExprNode trueExpr, ExprNode falseExpr) {
        this.cond = cond;
        this.trueExpr = trueExpr;
        this.falseExpr = falseExpr;
    }
}
