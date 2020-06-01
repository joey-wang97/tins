package cn.tianyu.tins.symbol;

import java.util.List;

/**
 * @author 汪继友
 * @since 2020/5/20
 */
public class SymbolTable {
    public static List<VarSymbol> varSymbols;
    public static List<StructSymbol> structSymbols;
    public static List<FuncSymbol> funcSymbols;

    public static void dumpSymbol() {
        varSymbols.forEach(System.out::println);
        structSymbols.forEach(System.out::println);
        funcSymbols.forEach(System.out::println);
    }
}
