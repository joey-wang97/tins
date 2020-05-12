package cn.tianyu.tins.ast.expr;

import cn.tianyu.tins.ast.ExprNode;
import cn.tianyu.tins.type.Token;

public class PrefixUnaryExpr extends ExprNode {
    public ExprNode expr;
    public Token.Type operator;

    public PrefixUnaryExpr(Token.Type operator,ExprNode expr) {
        this.expr = expr;
        this.operator = operator;
    }
}
