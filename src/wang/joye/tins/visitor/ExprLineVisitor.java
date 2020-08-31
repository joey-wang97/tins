package wang.joye.tins.visitor;

import wang.joye.tins.ast.node.ExprNode;

public class ExprLineVisitor {

    public static int getLine(ExprNode exprNode) {
        return exprNode.getLine();
    }
}
