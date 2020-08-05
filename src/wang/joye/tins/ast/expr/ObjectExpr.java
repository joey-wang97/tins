package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;

import java.util.LinkedList;
import java.util.List;

/**
 * 数组赋值语句
 * [1, 2.0, func1()]
 */
public class ObjectExpr extends FactorExpr {
    public List<ObjectField> fields;

    public ObjectExpr() {
        this.fields = new LinkedList<>();
    }

    public void addValue(ObjectField field) {
        this.fields.add(field);
    }

    public static class ObjectField {
        public String name;
        public ExprNode expr;
    }
}
