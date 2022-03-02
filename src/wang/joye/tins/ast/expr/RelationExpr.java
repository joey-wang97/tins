package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.ExprType;
import wang.joye.tins.type.Token;
import wang.joye.tins.util.DumpUtil;
import wang.joye.tins.visitor.ASTVisitor;

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
