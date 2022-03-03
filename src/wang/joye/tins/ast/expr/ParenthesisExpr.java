package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.util.DumpUtil;
import wang.joye.tins.visitor.ASTVisitor;

/**
 * 括号表达式
 */
public class ParenthesisExpr extends FactorExpr {
    public ExprNode expr;

    public ParenthesisExpr(ExprNode expr) {
        this.expr = expr;
    }

    @Override
    public void check(ASTVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, this);
    }
}
