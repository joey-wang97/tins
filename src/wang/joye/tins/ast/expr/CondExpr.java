package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.ExprType;
import wang.joye.tins.util.DumpUtil;
import wang.joye.tins.visitor.ASTVisitor;
import wang.joye.tins.visitor.ExprTypeVisitor;

public class CondExpr extends ExprNode {
    public ExprNode cond, trueExpr, falseExpr;

    public CondExpr(ExprNode cond, ExprNode trueExpr, ExprNode falseExpr) {
        this.cond = cond;
        this.trueExpr = trueExpr;
        this.falseExpr = falseExpr;
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level,this);
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
        return cond.getLine();
    }
}
