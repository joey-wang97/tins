package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.Token;
import wang.joye.tins.util.DumpUtil;

public class MulOrDivExpr extends ExprNode {
    public ExprNode leftExpr, rightExpr;
    public Token.Type operator;

    public MulOrDivExpr(ExprNode leftExpr, Token.Type operator, ExprNode rightExpr) {
        this.leftExpr = leftExpr;
        this.operator = operator;
        this.rightExpr = rightExpr;
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, "MulOrDivExpr");
        DumpUtil.dump(level + 1, "operator: " + operator.name());
        DumpUtil.dump(level + 1, "left expr");
        leftExpr.dump(level + 2);
        DumpUtil.dump(level + 1, "right expr");
        rightExpr.dump(level + 2);
    }
}
