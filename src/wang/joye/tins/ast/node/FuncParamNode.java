package wang.joye.tins.ast.node;

import wang.joye.tins.ASTVisitor;
import wang.joye.tins.type.Token;
import wang.joye.tins.util.DumpUtil;

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
    public void dump(int level) {
        String paramTypeName = paramTypeToken.name == null ? paramTypeToken.type.name() : paramTypeToken.name;
        DumpUtil.dump(level, "func param: " + paramTypeName + " " + paramNameToken.name
                + (dimensionLength > 0 ? "[" + dimensionLength + "]" : ""));

    }

    @Override
    public void visit(ASTVisitor visitor) {
        visitor.visit(this);
    }
}
