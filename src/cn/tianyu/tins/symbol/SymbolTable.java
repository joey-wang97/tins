package cn.tianyu.tins.symbol;

import java.util.LinkedList;
import java.util.List;

/**
 * @author 汪继友
 * @since 2020/5/20
 */
public class SymbolTable {
    public static List<VarSymbol> varSymbols = new LinkedList<>();
    public static List<StructSymbol> structSymbols = new LinkedList<>();
    public static List<FuncSymbol> funcSymbols = new LinkedList<>();

    public static void dumpSymbol() {
        varSymbols.forEach(System.out::println);
        structSymbols.forEach(System.out::println);
        funcSymbols.forEach(System.out::println);
    }
}
