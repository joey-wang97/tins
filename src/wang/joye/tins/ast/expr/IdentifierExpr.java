package wang.joye.tins.ast.expr;

import wang.joye.tins.util.DumpUtil;

/**
 * 标识符本身作为表达式
 */
public class IdentifierExpr extends FactorExpr {
    public String varName;

    public IdentifierExpr(String varName) {
        this.varName = varName;
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level,this);
    }
}
