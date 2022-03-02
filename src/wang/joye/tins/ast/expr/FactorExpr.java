package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.ExprType;
import wang.joye.tins.util.DumpUtil;
import wang.joye.tins.visitor.ASTVisitor;
import wang.joye.tins.visitor.ExprLineVisitor;
import wang.joye.tins.visitor.ExprTypeVisitor;

import java.util.List;

/**
 * factor expr
 * 根据产生式.
 */
public class FactorExpr extends ExprNode {
    // factor[0][1][2].factor[0][1].factor[1]
    // 有三个factorExpr，第一个包含三个arrExpr
    public ExprNode expr;
    public FactorExpr nextFactor;
    public List<ExprNode> arrIndexList;

    public FactorExpr() {
    }

    public FactorExpr(ExprNode expr) {
        this.expr = expr;
    }

    @Override
    public void visit(ASTVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, "factorExpr");
        expr.dump(level + 1);
        if (arrIndexList != null && arrIndexList.size() > 0) {
            DumpUtil.dump(level+1, "arrIndexList");
            arrIndexList.forEach(i -> i.dump(level + 2));
        }
        if (nextFactor != null) {
            DumpUtil.dump(level+1, "nextFactor");
            nextFactor.dump(level + 2);
        }
    }

    @Override
    public ExprType getType() {
        return ExprTypeVisitor.getType(this);
    }

    @Override
    public int getLine() {
        return ExprLineVisitor.getLine(this);
    }
}