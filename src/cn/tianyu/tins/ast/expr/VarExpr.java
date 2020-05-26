package cn.tianyu.tins.ast.expr;

import cn.tianyu.tins.ast.ExprNode;
import cn.tianyu.tins.type.Token;

public class VarExpr extends ExprNode {
    public String varName;

    public VarExpr(String varName) {
        this.varName = varName;
    }
}
