package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.util.DumpUtil;

public class ParenthesisExpr extends FactorExpr {
    public ExprNode expr;

    public ParenthesisExpr(ExprNode expr) {
        this.expr = expr;
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, this);
    }
}
