package wang.joye.tins.ast.node;

import wang.joye.tins.type.Token;
import wang.joye.tins.visitor.ASTVisitor;

import java.util.LinkedList;
import java.util.List;

/**
 * 变量定义节点
 */
public class VarDefNode extends Node {

    public Token varTypeToken;
    public Token varNameToken;
    public List<ExprNode> eachDimensionLength = new LinkedList<>();
    /**
     * 赋值表达式
     */
    public ExprNode value;

    /**
     * 添加数组维度
     */
    public void addDimensionLength(ExprNode length) {
        eachDimensionLength.add(length);
    }

    @Override
    public void check(ASTVisitor visitor) {
        visitor.visit(this);
    }
}