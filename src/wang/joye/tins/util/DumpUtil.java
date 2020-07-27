package wang.joye.tins.util;

public class DumpUtil {
    public static void dump(int level) {
        for (int i = 0; i < level * 2; i++) {
            System.out.print("-");
        }
    }

    public static void dump(int level, String str) {
        dump(level);
        System.out.println(str);
    }
}
