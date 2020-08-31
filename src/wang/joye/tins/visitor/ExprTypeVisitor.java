package wang.joye.tins.visitor;

import wang.joye.tins.ast.expr.AssignExpr;
import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.ExprType;

public class ExprTypeVisitor {

    public static ExprType getType(ExprNode exprNode) {
        return exprNode.type();
    }

    public static ExprType getType(AssignExpr assignExpr) {
        return null;
    }
}
