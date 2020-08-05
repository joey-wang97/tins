package wang.joye.tins.ast.node.stmt;

import wang.joye.tins.ast.node.StmtNode;
import wang.joye.tins.util.DumpUtil;

/**
 * 顶层结点
 */
public class EmptyStmtNode extends StmtNode {

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, "EMPTY");
    }
}
