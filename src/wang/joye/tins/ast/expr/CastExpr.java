package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.Token;
import wang.joye.tins.util.DumpUtil;

public class CastExpr extends ExprNode {
    public ExprNode expr;
    public Token castType;

    public CastExpr(Token castType, ExprNode expr) {
        this.castType = castType;
        this.expr = expr;
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, "Cast Expr");
        DumpUtil.dump(level + 1, "case type: " + castType.getTypeName());
        DumpUtil.dump(level + 1, "expr");
        expr.dump(level + 2);
    }
}
