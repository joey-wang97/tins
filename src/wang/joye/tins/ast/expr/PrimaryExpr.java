package wang.joye.tins.ast.expr;

import wang.joye.tins.type.Token;
import wang.joye.tins.util.DumpUtil;
import wang.joye.tins.visitor.ASTVisitor;

/**
 * 字面量表达式
 * 如整数，小数，字符和字符串
 */
public class PrimaryExpr extends FactorExpr {
    public Token token;

    public PrimaryExpr(Token token) {
        this.token = token;
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
