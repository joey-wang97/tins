package cn.tianyu.tins.ast.expr;

import cn.tianyu.tins.ast.ExprNode;
import cn.tianyu.tins.type.Token;

public class SuffixUnaryExpr extends ExprNode {
    public ExprNode expr;
    public Token.Type operator;

    public SuffixUnaryExpr(ExprNode expr, Token.Type operator) {
        this.expr = expr;
        this.operator = operator;
    }
}
