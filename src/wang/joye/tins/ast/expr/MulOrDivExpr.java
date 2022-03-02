package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.ExprType;
import wang.joye.tins.type.Token;
import wang.joye.tins.util.DumpUtil;
import wang.joye.tins.visitor.ASTVisitor;

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

    @Override
    public void check(ASTVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public ExprType getType() {
        int lw = leftExpr.getType().getWeight();
        int rw = rightExpr.getType().getWeight();
        return lw > rw ? leftExpr.getType() : rightExpr.getType();
    }

    @Override
    public int getLine() {
        return leftExpr.getLine();
    }
}
