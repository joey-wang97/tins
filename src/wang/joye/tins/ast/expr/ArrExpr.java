package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.util.DumpUtil;

import java.util.LinkedList;
import java.util.List;

/**
 * 数组赋值语句
 * [1, 2.0, func1()]
 */
public class ArrExpr extends FactorExpr {
    public List<ExprNode> exprs;

    public ArrExpr() {
        this.exprs = new LinkedList<>();
    }

    public ArrExpr(List<ExprNode> exprs) {
        this.exprs = exprs;
    }

    public void addValue(ExprNode exprNode) {
        this.exprs.add(exprNode);
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, "Arr Expr");
        for (int i = 0; i < exprs.size(); i++) {
            DumpUtil.dump(level+1, "value["+i+"]");
            exprs.get(i).dump(level+2);
        }
    }
}
