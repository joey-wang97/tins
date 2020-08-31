package wang.joye.tins.ast.node;

import wang.joye.tins.type.ExprType;

/**
 * 语法树的所有表达式均为ExprNode
 */
public abstract class ExprNode extends Node {

    /**
     * 返回表达式类型
     */
    public abstract ExprType type();

    public abstract int getLine();
}
