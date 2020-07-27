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
     * 数组维数
     */
    public int dimensionLength;

    @Override
    public void dump(int level) {
        String paramTypeName = paramType.name == null ? paramType.type.name() : paramType.name;
        DumpUtil.dump(level, "func param: " + paramTypeName + " " + paramName +
                (dimensionLength > 0 ? "[" + dimensionLength + "]" : ""));

    }
}
