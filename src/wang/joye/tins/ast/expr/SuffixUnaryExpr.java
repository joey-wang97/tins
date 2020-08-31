package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.Token;
import wang.joye.tins.util.DumpUtil;
import wang.joye.tins.visitor.ASTVisitor;

public class SuffixUnaryExpr extends ExprNode {
    public ExprNode expr;
    public Token.Type operator;

    public SuffixUnaryExpr(ExprNode expr, Token.Type operator) {
        this.expr = expr;
        this.operator = operator;
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, "SuffixUnaryExpr: " + operator.name());
        expr.dump(level + 1);
    }

    @Override
    public void check(ASTVisitor visitor) {
        visitor.visit(this);
    }
}
