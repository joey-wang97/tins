package wang.joye.tins.type;

import wang.joye.tins.SemanticCheck;
import wang.joye.tins.ast.node.VarDefNode;
import wang.joye.tins.util.ErrorUtil;

/**
 * 词法单元
 */
public class ExprType {
    public Type type;
    public String structName;
    // 数组维度
    public int arrDimension;

    public int getWeight() {
        return type.weight;
    }

    public String getName() {
        return type.name();
    }

    public ExprType(Type type) {
        this.type = type;
    }

    public String getTypeName() {
        return structName == null ? type.name() : structName;
    }

    public static ExprType convert2ExprType(Token token) {
        if (token.type == Token.Type.INT)
            return new ExprType(Type.INT);
        else if (token.type == Token.Type.STRING)
            return new ExprType(Type.STRING);
        else if (token.type == Token.Type.CHAR)
            return new ExprType(Type.CHAR);
        else if (token.type == Token.Type.DOUBLE)
            return new ExprType(Type.DOUBLE);
        else if (token.type == Token.Type.FLOAT)
            return new ExprType(Type.FLOAT);
        else if (token.type == Token.Type.IDENTIFIER) {
            // 检查符号表中是否有符号
            VarDefNode varDef = SemanticCheck.currScope.get(token.name);
            if (varDef == null) {
                ErrorUtil.error("can't find " + token.name + "symbol in table");
            }
            if (varDef.varTypeToken.type == Token.Type.STRUCT) {
                ExprType type = new ExprType(Type.STRUCT);
                type.structName = token.name;
                return type;
            }
            ErrorUtil.error("symbol " + token.name + " can't convert to ExprType");
        }
        ErrorUtil.error("can't convert " + token.type.name() + " to ExprType");
        return null;
    }

    /**
     * 表达式类型枚举
     */
    public enum Type {
        INT(1),
        LONG(3),
        STRING(5),
        CHAR(1),
        DOUBLE(4),
        FLOAT(2),
        STRUCT(-1);

        /**
         * 表达式类型的权重，越高的权重越优先
         * 权重用来做运算
         * 负数权重代表不能用来进行某些运算，如按位与、按位或
         */
        private final int weight = 0;

        Type(int weight) {

        }
    }
}
