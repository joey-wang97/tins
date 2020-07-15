package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.Token;

public class PrimaryExpr extends ExprNode {
    public Token token;

    public PrimaryExpr(Token token) {
        this.token = token;
    }
}
