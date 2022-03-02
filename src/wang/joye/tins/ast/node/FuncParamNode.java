package wang.joye.tins.ast.node;

import wang.joye.tins.type.Token;
import wang.joye.tins.visitor.ASTVisitor;

/**
 * 函数参数节点
 */
public class FuncParamNode extends Node {
    public Token paramTypeToken;
    public Token paramNameToken;
    /**
     * 数组维数
     */
    public int dimensionLength;
    /**
     * 可变数组
     */
    public boolean variableArr;

    @Override
    public void check(ASTVisitor visitor) {
        visitor.visit(this);
    }
}
