package wang.joye.tins.ast.node;

import wang.joye.tins.ast.stmt.CompoundStmtNode;
import wang.joye.tins.type.Token;
import wang.joye.tins.util.DumpUtil;

import java.util.List;

/**
 * 函数定义
 */
public class FuncDefNode extends Node {
    // 拥有函数的Struct: func Circle.area() -> owner is Circle
    public String owner;
    public Token funcType;
    public String funcName;
    public List<FuncParamNode> paramNode;
    public CompoundStmtNode bodyStmt;

    public void dump(int level) {
        String funcTypeName = funcType.name == null ? funcType.type.name() : funcType.name;

        // func void owner.name
        DumpUtil.dump(level, "func " + funcTypeName + " " + (owner != null ? owner + "." : "") + funcName);
        DumpUtil.dump(level, "func params:");
        paramNode.forEach(i -> i.dump(level + 1));
        DumpUtil.dump(level, "func stmts:");
        bodyStmt.stmts.forEach(i -> i.dump(level + 1));
    }
}