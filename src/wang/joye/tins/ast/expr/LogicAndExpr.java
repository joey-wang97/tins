package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;

public class LogicAndExpr extends ExprNode {
    public ExprNode leftExpr, rightExpr;

    public LogicAndExpr(ExprNode leftExpr, ExprNode rightExpr) {
        this.leftExpr = leftExpr;
        this.rightExpr = rightExpr;
    }
}
