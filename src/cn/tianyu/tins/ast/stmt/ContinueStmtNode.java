package cn.tianyu.tins.ast.stmt;

import cn.tianyu.tins.ast.StmtNode;

/**
 * 顶层结点
 */
public class ContinueStmtNode extends StmtNode {

    @Override
    public void dump(int indent) {
        dump(indent, "continue");
    }
}
