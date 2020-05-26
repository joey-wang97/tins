package cn.tianyu.tins.ast.expr;

import cn.tianyu.tins.ast.ExprNode;
import cn.tianyu.tins.type.Token;

public class CastExpr extends ExprNode {
    public ExprNode expr;
    public Token castType;

    public CastExpr(Token castType, ExprNode expr) {
        this.castType = castType;
        this.expr = expr;
    }
}
