package wang.joye.tins.visitor;

import wang.joye.tins.SemanticCheck;
import wang.joye.tins.ast.expr.*;
import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.ast.node.StructDefNode;
import wang.joye.tins.ast.node.VarDefNode;
import wang.joye.tins.type.ExprType;
import wang.joye.tins.util.ErrorUtil;

public class ExprTypeVisitor {

    public static ExprType getType(ExprNode exprNode) {
        return exprNode.getType();
    }

    public static ExprType getType(AssignExpr assignExpr) {
        return assignExpr.leftExpr.getType();
    }

    public static ExprType getType(AddOrSubExpr expr) {
        return expr.leftExpr.getType();
    }

    public static ExprType getType(ArrExpr expr) {
        if (expr.exprs.size() > 0)
            return expr.exprs.get(0).getType();
        ErrorUtil.error(expr.getLine(), "ArrExpr is empty");
        return null;
    }

    public static ExprType getType(BitAndExpr expr) {
        return expr.leftExpr.getType();
    }

    public static ExprType getType(BitOrExpr expr) {
        return expr.leftExpr.getType();
    }

    public static ExprType getType(BitXorExpr expr) {
        return expr.leftExpr.getType();
    }

    public static ExprType getType(CastExpr expr) {
        return ExprType.convert2ExprType(expr.castType);
    }

    public static ExprType getType(CondExpr expr) {
        return expr.trueExpr.getType();
    }

    public static ExprType getType(EqualityExpr expr) {
        return expr.leftExpr.getType();
    }

    public static ExprType getType(FactorExpr expr) {
        ExprType exprType = expr.expr.getType();
        // 如果有表达式：a.b.c
        // 则先取出a的类型，根据a的类型，得到属性b的类型
        // 根据a.b的类型，得到属性c的类型
        // 这里默认语义是正确的，正确性由SemanticCheck保证
        while (expr.nextFactor != null) {
            StructDefNode def = null;
            // 先找到结构体定义
            // a.b.c，先找a的定义
            for (StructDefNode structDef : SemanticCheck.globalAst.structDefNodes) {
                if (structDef.nameToken.name.equals(exprType.structName)) {
                    def = structDef;
                    break;
                }
            }
            // 这里必须用expr.nextFactor.expr，不能用expr.nextFactor.type()
            // 对于a.b.c来说，接下来计算的是b的type, 而不是b.c的type
            ExprType nextType = expr.nextFactor.expr.getType();
            for (VarDefNode fieldDef : def.fieldDefs) {
                if (fieldDef.varNameToken.name.equals(nextType.structName)) {

                }
            }

            // 求a.b.c -> 求b.c
            expr = expr.nextFactor;
        }

        // 如果expr取了数组: expr[0][1]，则减去取的维数
        // int[][][] a;
        // a[0][0];
        // 此时exprType为 INT型一维数组
        if (expr.arrIndexList.size() > 0) {
            exprType.arrDimension -= expr.arrIndexList.size();
        }
        return exprType;
    }

    public static ExprType getType(ExprType prefixType, FactorExpr factorExpr) {
        StructDefNode def;
        // 先找到结构体定义
        // a.b.c，先找a的定义
        for (StructDefNode structDef : SemanticCheck.globalAst.structDefNodes) {
            if (structDef.nameToken.name.equals(prefixType.structName)) {
                def = structDef;
                break;
            }
        }

        // TODO
        return null;
    }

    void visit(FactorExpr expr){}

    void visit(LogicAndExpr expr){}

    void visit(LogicOrExpr expr){}

    void visit(MulOrDivExpr expr){}

    void visit(ParenthesisExpr expr){}

    void visit(PrefixUnaryExpr expr){}

    void visit(PrimaryExpr expr){}

    void visit(RelationExpr expr){}

    void visit(ShiftExpr expr){}

    void visit(StructAssignExpr expr){}

    void visit(SuffixUnaryExpr expr){}

}
