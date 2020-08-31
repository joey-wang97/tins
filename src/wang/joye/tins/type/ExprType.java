package wang.joye.tins.type;

/**
 * 词法单元
 */
public class ExprType {
    public Type type;
    public String name;
    // 数组维度
    public int arrDimension;

    public ExprType() {
    }

    public ExprType(Type type) {
        this.type = type;
    }

    public String getTypeName() {
        return name == null ? type.name() : name;
    }

    public enum Type {
        INT,
        LONG,
        STRING,
        CHAR,
        DOUBLE,
        FLOAT,
        STRUCT,
    }
}
