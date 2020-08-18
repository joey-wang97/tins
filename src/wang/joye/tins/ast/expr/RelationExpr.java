package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.Token;
import wang.joye.tins.util.DumpUtil;

public class RelationExpr extends ExprNode {
    public ExprNode leftExpr, rightExpr;
    public Token.Type operator;

    public RelationExpr(ExprNode leftExpr, Token.Type operator, ExprNode rightExpr) {
        this.leftExpr = leftExpr;
        this.operator = operator;
        this.rightExpr = rightExpr;
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, "RelationExpr operator: "+ operator.name());
        DumpUtil.dump(level, "left Expr");
        leftExpr.dump(level+1);
        DumpUtil.dump(level, "right Expr");
        rightExpr.dump(level+1);
    }
}
