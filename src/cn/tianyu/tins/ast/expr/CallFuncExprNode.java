package cn.tianyu.tins.ast.expr;

import cn.tianyu.tins.ast.ExprNode;
import cn.tianyu.tins.type.Token;

import java.util.List;

public class CallFuncExprNode extends ExprNode {
    public List<ExprNode> params;
    public String funcName;

    public CallFuncExprNode(String funcName, List<ExprNode> params) {
        this.funcName = funcName;
        this.params = params;
    }
}
