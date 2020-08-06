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
        DumpUtil.dump(level, "CallFuncExpr: " + funcName);
        for (int i = 0; i < params.size(); i++) {
            DumpUtil.dump(level + 1, "param[" + i + "]");
            params.get(i).dump(level + 2);
        }
    }
}
