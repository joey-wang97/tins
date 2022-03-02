package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.ExprType;
import wang.joye.tins.util.DumpUtil;
import wang.joye.tins.util.ErrorUtil;
import wang.joye.tins.visitor.ASTVisitor;

public class BitXorExpr extends ExprNode {
    public ExprNode leftExpr, rightExpr;

    public BitXorExpr(ExprNode leftExpr, ExprNode rightExpr) {
        this.leftExpr = leftExpr;
        this.rightExpr = rightExpr;
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, "Bit Xor Expr");
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
        int lw = leftExpr.getType().getWeight();
        int rw = rightExpr.getType().getWeight();
        String leftTypeName = leftExpr.getType().getName();
        String rightTypeName = leftExpr.getType().getName();
        if (lw < 0 || rw < 0) {
            ErrorUtil.error(getLine(), leftTypeName + "与" + rightTypeName + "不能进行按位异或操作");
        }
        return leftExpr.getType();
    }

    @Override
    public int getLine() {
        return leftExpr.getLine();
    }
}
