package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;

public class LogicOrExpr extends ExprNode {
    public ExprNode leftExpr, rightExpr;

    public LogicOrExpr(ExprNode leftExpr, ExprNode rightExpr) {
        this.leftExpr = leftExpr;
        this.rightExpr = rightExpr;
    }
}
