package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;

/**
 * 标识符本身作为表达式
 */
public class IdentifierExpr extends ExprNode {
    public String varName;

    public IdentifierExpr(String varName) {
        this.varName = varName;
    }
}
