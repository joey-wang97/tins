package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;

import java.util.LinkedList;
import java.util.List;

/**
 * 数组赋值语句
 * [1, 2.0, func1()]
 */
public class ArrExpr extends ExprNode {
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
}
