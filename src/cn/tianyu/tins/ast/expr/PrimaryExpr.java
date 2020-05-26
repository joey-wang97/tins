package cn.tianyu.tins.ast.expr;

import cn.tianyu.tins.ast.ExprNode;
import cn.tianyu.tins.type.Token;

public class PrimaryExpr extends ExprNode {
    public Token token;

    public PrimaryExpr(Token token) {
        this.token = token;
    }
}
