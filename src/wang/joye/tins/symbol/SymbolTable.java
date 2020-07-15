package wang.joye.tins.symbol;

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

    public static void addStruct(String name) {
        structSymbols.forEach(i->{
            if (i.name.equals(name))
                error("duplicate struct name!");
        });
        StructSymbol symbol = new StructSymbol();
        symbol.name = name;
        structSymbols.add(symbol);
    }

    public static void error(String msg) {
        System.err.println(msg);
        System.exit(-1);
    }
}
