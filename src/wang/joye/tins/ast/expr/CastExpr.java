package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.Token;

public class CastExpr extends ExprNode {
    public ExprNode expr;
    public Token castType;

    public CastExpr(Token castType, ExprNode expr) {
        this.castType = castType;
        this.expr = expr;
    }
}
