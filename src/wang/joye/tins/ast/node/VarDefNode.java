package wang.joye.tins.ast.node;

import wang.joye.tins.type.Token;
import wang.joye.tins.util.DumpUtil;
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
    // 赋值表达式
    public ExprNode value;

    /**
     * 添加数组维度
     */
    public void addDimensionLength(ExprNode length) {
        eachDimensionLength.add(length);
    }

    @Override
    public void dump(int level) {
        String varTypeName = varTypeToken.name == null ? varTypeToken.type.name() : varTypeToken.name;
        // 维度描述
        StringBuilder dimensionStr = new StringBuilder();
        for (int i = 0; i < eachDimensionLength.size(); i++) {
            dimensionStr.append("[]");
        }
        DumpUtil.dump(level, "var: " + varTypeName + " " + varNameToken + dimensionStr);
        for (int i = 0; i < eachDimensionLength.size(); i++) {
            DumpUtil.dump(level + 1, "dimension[" + i + "]:");
            if (eachDimensionLength.get(i) == null) {
                DumpUtil.dump(level+2, "empty dimension");
            } else
                eachDimensionLength.get(i).dump(level + 2);
        }
        if (value != null)
            value.dump(level + 1);
    }

    @Override
    public void check(ASTVisitor visitor) {
        visitor.visit(this);
    }
}