package wang.joye.tins.visitor;

import wang.joye.tins.ast.expr.*;
import wang.joye.tins.ast.node.*;
import wang.joye.tins.ast.stmt.*;

public interface ASTVisitor {

    void visit(AddOrSubExpr expr);

    void visit(ArrExpr expr);

    void visit(AssignExpr expr);

    void visit(BitAndExpr expr);

    void visit(BitOrExpr expr);

    void visit(BitXorExpr expr);

    void visit(CastExpr castExpr);

    void visit(CondExpr expr);

    void visit(EqualityExpr expr);

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

    void visit(BreakStmtNode stmt);

    void visit(CaseStmtNode stmt);

    void visit(CompoundStmtNode stmt);

    void visit(ContinueStmtNode stmt);

    void visit(EmptyStmtNode stmt);

    void visit(ExprStmtNode stmt);

    void visit(ForStmtNode stmt);

    void visit(IfStmtNode stmt);

    void visit(ReturnStmtNode stmt);

    void visit(SwitchStmtNode stmt);

    void visit(VarDefStmtNode stmt);

    void visit(WhileStmtNode stmt);

    void visit(ImportNode importNode);

    void visit(StructDefNode structDefNode);

    void visit(VarDefNode varDefNode);

    void visit(FuncDefNode funcDefNode);

    void visit(FuncParamNode funcParamNode);
}
