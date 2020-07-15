package wang.joye.tins.ast.node;

import wang.joye.tins.type.Token;
import wang.joye.tins.util.DumpUtil;

/**
 * 函数参数节点
 */
public class FuncParamNode extends Node {
    public Token paramType;
    public String paramName;
    /**
     * 每个维数的长度
     * int[3][4] a
     * a有两个维数，第一维是3，第二维是4
     */
    public ExprNode[] eachDimensionLength;

    @Override
    public void dump(int level) {
        String paramTypeName = paramType.name == null ? paramType.type.name() : paramType.name;
        // 维度描述
        StringBuilder dimensionStr = new StringBuilder();
        for (int i = 0; i < eachDimensionLength.length; i++) {
            dimensionStr.append("[]");
        }
        DumpUtil.dump(level, "func param: " + paramTypeName + " " + paramName + dimensionStr.toString());

        for (int i = 0; i < eachDimensionLength.length; i++) {
            DumpUtil.dump(level + 1, "dimension[" + i + "]:");
            eachDimensionLength[i].dump(level + 2);
        }
    }
}
