package wang.joye.tins.ast.node.stmt;

import wang.joye.tins.ast.node.StmtNode;
import wang.joye.tins.util.DumpUtil;

/**
 * 顶层结点
 */
public class BreakStmtNode extends StmtNode {

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, "BREAK");
    }
}
