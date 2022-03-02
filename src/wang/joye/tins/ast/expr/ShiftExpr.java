package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.ExprType;
import wang.joye.tins.type.Token;
import wang.joye.tins.util.DumpUtil;
import wang.joye.tins.visitor.ASTVisitor;

public class ShiftExpr extends ExprNode {
    public ExprNode leftExpr, rightExpr;
    public Token.Type operator;

    public ShiftExpr(ExprNode leftExpr, Token.Type operator, ExprNode rightExpr) {
        this.leftExpr = leftExpr;
        this.operator = operator;
        this.rightExpr = rightExpr;
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, "Shift Expr");
        DumpUtil.dump(level+1, "operator: "+ operator.name());
        DumpUtil.dump(level+1, "left Expr");
        leftExpr.dump(level+2);
        DumpUtil.dump(level+1, "right Expr");
        rightExpr.dump(level+2);
    }

    @Override
    public void check(ASTVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public ExprType getType() {
        return leftExpr.getType();
    }

    @Override
    public int getLine() {
        return leftExpr.getLine();
    }
}
