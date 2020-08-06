package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.Token;
import wang.joye.tins.util.DumpUtil;

public class PrefixUnaryExpr extends ExprNode {
    public ExprNode expr;
    public Token.Type operator;

    public PrefixUnaryExpr(Token.Type operator,ExprNode expr) {
        this.expr = expr;
        this.operator = operator;
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, "Prefix Unary Expr");
        DumpUtil.dump(level+1, "operator: "+ operator.name());
        DumpUtil.dump(level+1, "expr");
        expr.dump(level+2);
    }
}
