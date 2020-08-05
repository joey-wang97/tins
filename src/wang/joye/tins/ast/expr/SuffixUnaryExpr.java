package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.Token;

public class SuffixUnaryExpr extends ExprNode {
    public ExprNode expr;
    public Token.Type operator;

    public SuffixUnaryExpr(ExprNode expr, Token.Type operator) {
        this.expr = expr;
        this.operator = operator;
    }

    @Override
    public void dump(int level) {

    }
}
