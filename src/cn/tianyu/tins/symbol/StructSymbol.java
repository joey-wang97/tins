package cn.tianyu.tins.symbol;

import java.util.ArrayList;
import java.util.List;

/**
 * 结构体
 */
public class StructSymbol {
    public String name;
    public List<VarSymbol> fieldSymbols = new ArrayList<>();

    @Override
    public String toString() {
        return "StructSymbol{" +
                "name='" + name + '\'' +
                ", fieldSymbols=" + fieldSymbols.size() +
                '}';
    }
}
