package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.util.DumpUtil;

import java.util.List;

public class CallFuncExprNode extends FactorExpr {
    public List<ExprNode> params;
    public String funcName;

    public CallFuncExprNode(String funcName, List<ExprNode> params) {
        this.funcName = funcName;
        this.params = params;
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, this);
    }
}
