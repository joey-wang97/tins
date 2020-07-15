package wang.joye.tins.ast.stmt;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.ast.node.StmtNode;

/**
 * 顶层结点
 */
public class WhileStmtNode extends StmtNode {

    public ExprNode condition;
    public StmtNode stmt;

    @Override
    public void dump(int level) {
        super.printIndent(level);
        System.out.println();
    }
}
