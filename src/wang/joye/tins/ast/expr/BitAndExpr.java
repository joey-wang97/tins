package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.ExprType;
import wang.joye.tins.util.DumpUtil;
import wang.joye.tins.visitor.ASTVisitor;
import wang.joye.tins.visitor.ExprLineVisitor;
import wang.joye.tins.visitor.ExprTypeVisitor;

public class BitAndExpr extends ExprNode {
    public ExprNode leftExpr, rightExpr;

    public BitAndExpr(ExprNode leftExpr, ExprNode rightExpr) {
        this.leftExpr = leftExpr;
        this.rightExpr = rightExpr;
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, this);
    }

    @Override
    public void check(ASTVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public ExprType getType() {
        return ExprTypeVisitor.getType(this);
    }

    @Override
    public int getLine() {
        return ExprLineVisitor.getLine(this);
    }
}
