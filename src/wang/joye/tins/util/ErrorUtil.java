package wang.joye.tins.util;

public class ErrorUtil {
    public static void error(int line, String message) {
        System.err.println("at line " + line + ": " + message);
        System.exit(-1);
    }

    public static void error(String message) {
        System.err.println(message);
        System.exit(-1);
    }
}
