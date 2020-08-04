package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;

import java.util.List;

/**
 * factor expr
 * 根据产生式.
 */
public class FactorExpr extends ExprNode {
    public ExprNode expr;
    List<NextExpr> nextExprs;

    public FactorExpr(ExprNode expr, List<NextExpr> nextExprs) {
        this.expr = expr;
        this.nextExprs = nextExprs;
    }

    /**
     * factor的下一个操作符
     * 只能取field和arrIndex其中之一
     */
    public static class NextExpr {
        public ExprNode arrIndex;

        public NextExpr(ExprNode arrIndex) {
            this.arrIndex = arrIndex;
        }
    }
}