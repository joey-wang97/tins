package cn.tianyu.tins.ast.expr;

import cn.tianyu.tins.ast.ExprNode;
import cn.tianyu.tins.type.Token;

/**
 * 标识符本身作为表达式
 */
public class IdentifierExpr extends ExprNode {
    public String varName;

    public IdentifierExpr(String varName) {
        this.varName = varName;
    }
}
