package wang.joye.tins.ast.node;

import wang.joye.tins.type.Token;
import wang.joye.tins.util.DumpUtil;

/**
 * 变量定义节点
 */
public class VarDefNode extends Node {

    public Token varType;
    public String varName;
    public ExprNode[] eachDimensionLength;
    public ExprNode value;

    @Override
    public void dump(int level) {
        String varTypeName = varType.name == null ? varType.type.name() : varType.name;
        // 维度描述
        StringBuilder dimensionStr = new StringBuilder();
        for (int i = 0; i < eachDimensionLength.length; i++) {
            dimensionStr.append("[]");
        }
        DumpUtil.dump(level, "var: " + varTypeName + " " + varName + dimensionStr);
        for (int i = 0; i < eachDimensionLength.length; i++) {
            DumpUtil.dump(level + 1, "dimension[" + i + "]:");
            eachDimensionLength[i].dump(level + 2);
        }
        value.dump(level+1);
    }
}
