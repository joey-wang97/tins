package cn.tianyu.tins.ast.expr;

import cn.tianyu.tins.ast.ExprNode;
import cn.tianyu.tins.type.Token;

public class ParenthesisExpr extends ExprNode {
    public ExprNode expr;

    public ParenthesisExpr(ExprNode expr) {
        this.expr = expr;
    }
}
