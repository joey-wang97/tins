package wang.joye.tins.ast.node;

import wang.joye.tins.ast.stmt.CompoundStmtNode;
import wang.joye.tins.type.Token;
import wang.joye.tins.visitor.ASTVisitor;

import java.util.LinkedList;
import java.util.List;

/**
 * 函数定义
 */
public class FuncDefNode extends Node {
    /**
     * 拥有函数的Struct: func Circle.area() -> owner is Circle
     */
    public String owner;
    /**
     * 函数返回类型
     */
    public Token funcTypeToken;
    public Token funcNameToken;
    public List<ExprNode> eachDimensionLength = new LinkedList<>();
    public List<FuncParamNode> paramNode;
    public CompoundStmtNode bodyStmt;

    @Override
    public void check(ASTVisitor visitor) {
        visitor.visit(this);
    }
}
