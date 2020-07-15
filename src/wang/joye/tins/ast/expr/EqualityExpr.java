package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.Token;

public class EqualityExpr extends ExprNode {
    public ExprNode leftExpr, rightExpr;
    public Token.Type operator;

    public EqualityExpr(ExprNode leftExpr, Token.Type operator, ExprNode rightExpr) {
        this.leftExpr = leftExpr;
        this.operator = operator;
        this.rightExpr = rightExpr;
    }
}
