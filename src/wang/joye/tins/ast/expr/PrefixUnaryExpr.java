package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.Token;

public class PrefixUnaryExpr extends ExprNode {
    public ExprNode expr;
    public Token.Type operator;

    public PrefixUnaryExpr(Token.Type operator,ExprNode expr) {
        this.expr = expr;
        this.operator = operator;
    }
}
