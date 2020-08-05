package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;

public class ParenthesisExpr extends FactorExpr {
    public ExprNode expr;

    public ParenthesisExpr(ExprNode expr) {
        this.expr = expr;
    }
}
