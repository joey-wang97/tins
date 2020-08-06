package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.util.DumpUtil;

import java.util.LinkedList;
import java.util.List;

/**
 * 对象
 * [1, 2.0, func1()]
 */
public class StructAssignExpr extends FactorExpr {
    public List<ObjectField> fields;

    public StructAssignExpr() {
        this.fields = new LinkedList<>();
    }

    public void addValue(ObjectField field) {
        this.fields.add(field);
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, "Struct Assign Expr");
        for (int i = 0; i < fields.size(); i++) {
            DumpUtil.dump(level + 1, "fields[" + i + "]: " + fields.get(i).name);
            DumpUtil.dump(level + 1, "expr");
            fields.get(i).expr.dump(level + 2);
        }
    }

    public static class ObjectField {
        public String name;
        public ExprNode expr;
    }
}
