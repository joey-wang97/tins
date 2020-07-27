package wang.joye.tins.ast.stmt;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.ast.node.StmtNode;

/**
 * 顶层结点
 */
public class ForStmtNode extends StmtNode {

    public ExprNode init,condition, operation;
    public StmtNode stmt;

    @Override
    public void dump(int level) {
        // TODO
    }
}
