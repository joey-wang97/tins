package wang.joye.tins.ast.expr;

import wang.joye.tins.ast.node.ExprNode;
import wang.joye.tins.type.ExprType;
import wang.joye.tins.type.Token;
import wang.joye.tins.util.DumpUtil;
import wang.joye.tins.visitor.ASTVisitor;

public class EqualityExpr extends ExprNode {
    public ExprNode leftExpr, rightExpr;
    public Token.Type operator;

    public EqualityExpr(ExprNode leftExpr, Token.Type operator, ExprNode rightExpr) {
        this.leftExpr = leftExpr;
        this.operator = operator;
        this.rightExpr = rightExpr;
    }

    @Override
    public void dump(int level) {
        DumpUtil.dump(level,this);
    }

    @Override
    public void check(ASTVisitor visitor) {
        visitor.visit(this);
    }

    /**
     * 比较表达式返回0和1
     * @return
     */
    @Override
    public ExprType getType() {
        return new ExprType(ExprType.Type.INT);
    }

    @Override
    public int getLine() {
        return 0;
    }
}
