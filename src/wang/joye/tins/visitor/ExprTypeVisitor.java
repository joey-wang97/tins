package wang.joye.tins.visitor;

import wang.joye.tins.ast.expr.*;
import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.ExprType;
import wang.joye.tins.util.ErrorUtil;

public class ExprTypeVisitor {

    public static ExprType getType(ExprNode exprNode) {
        return exprNode.type();
    }

    public static ExprType getType(AssignExpr assignExpr) {
        return assignExpr.leftExpr.type();
    }

    public static ExprType getType(AddOrSubExpr expr) {
        return expr.leftExpr.type();
    }

    public static ExprType getType(ArrExpr expr) {
        if (expr.exprs.size() > 0)
            return expr.exprs.get(0).type();
        ErrorUtil.error(expr.getLine(), "ArrExpr is empty");
        return null;
    }

    public static ExprType getType(BitAndExpr expr) {
        return expr.leftExpr.type();
    }

    public static ExprType getType(BitOrExpr expr) {
        return expr.leftExpr.type();
    }

    public static ExprType getType(BitXorExpr expr) {
        return expr.leftExpr.type();
    }

    public static ExprType getType(CastExpr expr) {
        return ExprType.convert2ExprType(expr.castType);
    }

    public static ExprType getType(CondExpr expr) {
        return expr.trueExpr.type();
    }

    public static ExprType getType(EqualityExpr expr) {
        return expr.leftExpr.type();
    }

    public static ExprType getType(FactorExpr expr) {
        // TODO factorExpr type
        // return expr.leftExpr.type();
    }

    void visit(FactorExpr expr);

    void visit(LogicAndExpr expr);

    void visit(LogicOrExpr expr);

    void visit(MulOrDivExpr expr);

    void visit(ParenthesisExpr expr);

    void visit(PrefixUnaryExpr expr);

    void visit(PrimaryExpr expr);

    void visit(RelationExpr expr);

    void visit(ShiftExpr expr);

    void visit(StructAssignExpr expr);

    void visit(SuffixUnaryExpr expr);

}
