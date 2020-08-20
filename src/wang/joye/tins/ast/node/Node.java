package wang.joye.tins.ast.node;

import wang.joye.tins.ASTVisitor;

/**
 * 语法树的所有节点均为Node类
 */
public abstract class Node {

    public abstract void dump(int level);

    public abstract void visit(ASTVisitor visitor);
}
