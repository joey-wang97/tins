package wang.joye.tins.symbol;

import wang.joye.tins.type.Token;

import java.util.List;

/**
 * 顶层结点
 */
public class FuncSymbol {
    public Token funcType;
    public String funcName;
    public List<VarSymbol> params;

    @Override
    public String toString() {
        return "FuncSymbol{" +
                "funcType=" + funcType +
                ", funcName='" + funcName + '\'' +
                ", params=" + params.size() +
                '}';
    }
}
