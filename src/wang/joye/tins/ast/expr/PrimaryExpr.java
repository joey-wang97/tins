package wang.joye.tins.ast.expr;

import wang.joye.tins.type.Token;
import wang.joye.tins.util.DumpUtil;

public class PrimaryExpr extends FactorExpr {
    public Token token;

    public PrimaryExpr(Token token) {
        this.token = token;
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, this);
    }
}
