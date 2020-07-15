package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;

public class CondExpr extends ExprNode {
    public ExprNode cond, trueExpr, falseExpr;

    public CondExpr(ExprNode cond, ExprNode trueExpr, ExprNode falseExpr) {
        this.cond = cond;
        this.trueExpr = trueExpr;
        this.falseExpr = falseExpr;
    }
}
