package wang.joye.tins.ast.node;

import wang.joye.tins.type.Token;
import wang.joye.tins.util.DumpUtil;

import java.util.LinkedList;
import java.util.List;

/**
 * 变量定义节点
 */
public class VarDefNode extends Node {

    public Token varType;
    public String varName;
    public List<ExprNode> eachDimensionLength = new LinkedList<>();
    public ExprNode value;

    /**
     * 添加数组维度
     */
    public void addDimensionLength(ExprNode length) {
        eachDimensionLength.add(length);
    }

    @Override
    public void dump(int level) {
        String varTypeName = varType.name == null ? varType.type.name() : varType.name;
        // 维度描述
        StringBuilder dimensionStr = new StringBuilder();
        for (int i = 0; i < eachDimensionLength.size(); i++) {
            dimensionStr.append("[]");
        }
        DumpUtil.dump(level, "var: " + varTypeName + " " + varName + dimensionStr);
        for (int i = 0; i < eachDimensionLength.size(); i++) {
            DumpUtil.dump(level + 1, "dimension[" + i + "]:");
            eachDimensionLength.get(i).dump(level + 2);
        }
        if (value != null)
            value.dump(level + 1);
    }
}
