package cn.tianyu.tins.symbol;

import cn.tianyu.tins.type.Token;

/**
 * 顶层结点
 */
public class VarSymbol {

    public Token varType;
    public String varName;
    public boolean isArr;
    public Object value;

    @Override
    public String toString() {
        return "VarSymbol{" +
                "varType=" + varType +
                ", varName='" + varName + '\'' +
                ", isArr=" + isArr +
                ", value=" + value +
                '}';
    }
}
