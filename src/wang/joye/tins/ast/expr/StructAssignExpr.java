package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.Token;
import wang.joye.tins.util.DumpUtil;

import java.util.LinkedList;
import java.util.List;

/**
 * 对象
 * [1, 2.0, func1()]
 */
public class StructAssignExpr extends FactorExpr {
    public List<ObjectField> fieldValues = new LinkedList<>();

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, this);
    }

    public static class ObjectField {
        public Token nameToken;
        public ExprNode expr;
    }
}
