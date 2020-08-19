package wang.joye.tins;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.Token;

public class ExprUtil {

    public static int getLine(ExprNode node) {
        return 1;
    }

    /**
     * 检查token和expr的类型是否一致，允许隐式转换
     */
    public static void checkMatch(Token token, ExprNode exprNode) {

    }

    /**
     * 判断两个token是否同一变量类型
     */
    public static boolean sameTokenType(Token token1, Token token2) {
        // 如果都是基本类型
        if (token1.type == token2.type && token1.type != Token.Type.IDENTIFIER)
            return true;
        // 如果是同一结构体类型
        if (token1.type == Token.Type.IDENTIFIER && token2.type == Token.Type.IDENTIFIER
                && token1.name.equals(token2.name))
            return true;
        return false;
    }

}
