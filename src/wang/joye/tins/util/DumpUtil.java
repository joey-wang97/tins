package wang.joye.tins.util;

import wang.joye.tins.ast.AST;
import wang.joye.tins.ast.expr.*;
import wang.joye.tins.ast.node.*;
import wang.joye.tins.ast.stmt.*;

/**
 * 使用DumpUtil输出AST结构
 * 对于未确定的类型，如ExprNode, StmtNode。它们的类型只能在运行时确定，所以必须通过重载实现
 * 重载后再跳转DumpUtil进行输出
 */
public class DumpUtil {
    // TODO 修改名字，修改参数名字
    private static void dump(int level) {
        for (int i = 0; i < level * 2; i++) {
            System.out.print("-");
        }
    }

    private static void dump(int level, String str) {
        dump(level);
        System.out.println(str);
    }

    private static void dump(String str) {
        System.out.println(str);
    }

    public static void dump(AST ast) {
        dump("import list");
        ast.importNodes.forEach(i -> dump(1, i));
        dump("");
        dump("struct def list");
        ast.structDefNodes.forEach(i -> dump(1, i));
        dump("");
        dump("var def list");
        ast.varDefNodes.forEach(i -> dump(1, i));
        dump("");
        dump("func def list");
        ast.funcDefNodes.forEach(i -> dump(1, i));
    }

    public static void dump(int level, ImportNode importNode) {
        dump(level);
        StringBuilder builder = new StringBuilder();
        importNode.folders.forEach(i -> builder.append(i).append(" "));
        dump(builder.toString());
    }

    public static void dump(int level, StructDefNode structDefNode) {
        dump(level, "struct name: " + structDefNode.nameToken);
        dump(level, "struct fields:");
        structDefNode.fieldDefs.forEach(i -> dump(level + 1, i));
        dump("");
    }

    public static void dump(int level, VarDefNode varDefNode) {
        String varTypeName = varDefNode.varTypeToken.name == null
                ? varDefNode.varTypeToken.type.name()
                : varDefNode.varTypeToken.name;
        // 维度描述
        StringBuilder dimensionStr = new StringBuilder();
        for (int i = 0; i < varDefNode.eachDimensionLength.size(); i++) {
            dimensionStr.append("[]");
        }
        DumpUtil.dump(level, "var: " + varTypeName + " " + varDefNode.varNameToken + dimensionStr);
        for (int i = 0; i < varDefNode.eachDimensionLength.size(); i++) {
            DumpUtil.dump(level + 1, "dimension[" + i + "]:");
            if (varDefNode.eachDimensionLength.get(i) == null) {
                DumpUtil.dump(level + 2, "empty dimension");
            } else {
                varDefNode.eachDimensionLength.get(i).dump(level + 2);
            }
        }
        if (varDefNode.value != null) {
            varDefNode.value.dump(level + 1);
        }
    }

    public static void dump(int level, FuncDefNode funcDefNode) {
        String funcTypeName = funcDefNode.funcTypeToken.name == null
                ? funcDefNode.funcTypeToken.type.name()
                : funcDefNode.funcTypeToken.name;

        // func void owner.name
        dump(level, "func " + funcTypeName + " " + (funcDefNode.owner != null
                ? funcDefNode.owner + "."
                : "") + funcDefNode.funcNameToken);
        dump(level, "func params:");
        funcDefNode.paramNode.forEach(i -> dump(level + 1, i));
        DumpUtil.dump(level, "func stmts:");
        funcDefNode.bodyStmt.stmts.forEach(i -> i.dump(level + 1));
    }

    public static void dump(int level, FuncParamNode funcParamNode) {
        String paramTypeName = funcParamNode.paramTypeToken.name == null
                ? funcParamNode.paramTypeToken.type.name()
                : funcParamNode.paramTypeToken.name;
        DumpUtil.dump(level, "func param: " + paramTypeName + " " + funcParamNode.paramNameToken.name
                + (funcParamNode.dimensionLength > 0 ? "[" + funcParamNode.dimensionLength + "]" : ""));

    }

    public static void dump(int level, CompoundStmtNode compoundStmtNode) {
        dump(level, "compound stmts:");
        compoundStmtNode.stmts.forEach(i -> i.dump(level + 1));
    }

    public static void dump(int level, AddOrSubExpr addOrSubExpr) {
        dump(level, "expr: " + addOrSubExpr.operator.name());
        DumpUtil.dump(level, "left expr");
        addOrSubExpr.leftExpr.dump(level + 1);
        DumpUtil.dump(level, "right expr");
        addOrSubExpr.rightExpr.dump(level + 1);
    }

    public static void dump(int level, ArrExpr arrExpr) {
        DumpUtil.dump(level, "Arr Expr");
        for (int i = 0; i < arrExpr.exprs.size(); i++) {
            DumpUtil.dump(level+1, "value["+i+"]");
            arrExpr.exprs.get(i).dump(level+2);
        }
    }

    public static void dump(int level, AssignExpr assignExpr) {
        DumpUtil.dump(level, "Assign Expr: "+ assignExpr.operator.name());
        DumpUtil.dump(level+1, "left Expr");
        assignExpr.leftExpr.dump(level+2);
        DumpUtil.dump(level+1, "right Expr");
        assignExpr.rightExpr.dump(level+2);
    }

    public static void dump(int level, BitAndExpr bitAndExpr) {
        DumpUtil.dump(level, "Bit And Expr");
        DumpUtil.dump(level + 1, "left Expr");
        bitAndExpr.leftExpr.dump(level + 2);
        DumpUtil.dump(level + 1, "right Expr");
        bitAndExpr.rightExpr.dump(level + 2);
    }

    public static void dump(int level, BitOrExpr bitOrExpr) {
        DumpUtil.dump(level, "Bit Or Expr");
        DumpUtil.dump(level + 1, "left Expr");
        bitOrExpr.leftExpr.dump(level + 2);
        DumpUtil.dump(level + 1, "right Expr");
        bitOrExpr.rightExpr.dump(level + 2);
    }

    public static void dump(int level, BitXorExpr bitXorExpr) {
        DumpUtil.dump(level, "Bit Xor Expr");
        DumpUtil.dump(level+1, "left Expr");
        bitXorExpr.leftExpr.dump(level+2);
        DumpUtil.dump(level+1, "right Expr");
        bitXorExpr.rightExpr.dump(level+2);
    }

    public static void dump(int level, CallFuncExprNode callFuncExprNode) {
        DumpUtil.dump(level, "CallFuncExpr: " + callFuncExprNode.funcName);
        for (int i = 0; i < callFuncExprNode.params.size(); i++) {
            DumpUtil.dump(level + 1, "param[" + i + "]");
            callFuncExprNode.params.get(i).dump(level + 2);
        }
    }

    public static void dump(int level, CastExpr castExpr) {
        DumpUtil.dump(level, "Cast Expr");
        DumpUtil.dump(level + 1, "case type: " + castExpr.castType.getTypeName());
        DumpUtil.dump(level + 1, "expr");
        castExpr.expr.dump(level + 2);
    }

    public static void dump(int level, CondExpr condExpr) {
        DumpUtil.dump(level, "Cond Expr");
        DumpUtil.dump(level + 1, "cond");
        condExpr.cond.dump(level + 2);
        DumpUtil.dump(level + 1, "true expr");
        condExpr.trueExpr.dump(level + 2);
        DumpUtil.dump(level + 1, "false expr");
        condExpr.falseExpr.dump(level + 2);
    }

    public static void dump(int level, EqualityExpr equalityExpr) {
        DumpUtil.dump(level, "Equality Expr");
        DumpUtil.dump(level + 1, "operator: " + equalityExpr.operator.name());
        DumpUtil.dump(level + 1, "left expr");
        equalityExpr.leftExpr.dump(level + 2);
        DumpUtil.dump(level + 1, "right expr");
        equalityExpr.rightExpr.dump(level + 2);
    }

    public static void dump(int level, FactorExpr factorExpr) {
        DumpUtil.dump(level, "factorExpr");
        factorExpr.expr.dump(level + 1);
        if (factorExpr.arrIndexList != null && factorExpr.arrIndexList.size() > 0) {
            DumpUtil.dump(level+1, "arrIndexList");
            factorExpr.arrIndexList.forEach(i -> i.dump(level + 2));
        }
        if (factorExpr.nextFactor != null) {
            DumpUtil.dump(level+1, "nextFactor");
            factorExpr.nextFactor.dump(level + 2);
        }
    }

    public static void dump(int level, IdentifierExpr identifierExpr) {
        DumpUtil.dump(level, "IdentifierExpr: " + identifierExpr.varName);
    }

    public static void dump(int level, LogicAndExpr logicAndExpr) {
        DumpUtil.dump(level, "Logic And Expr");
        DumpUtil.dump(level+1, "left Expr");
        logicAndExpr.leftExpr.dump(level+2);
        DumpUtil.dump(level+1, "right Expr");
        logicAndExpr.rightExpr.dump(level+2);
    }

    public static void dump(int level, LogicOrExpr logicOrExpr) {
        DumpUtil.dump(level, "Logic Or Expr");
        DumpUtil.dump(level+1, "left Expr");
        logicOrExpr.leftExpr.dump(level+2);
        DumpUtil.dump(level+1, "right Expr");
        logicOrExpr.rightExpr.dump(level+2);
    }

    public static void dump(int level, MulOrDivExpr mulOrDivExpr) {
        DumpUtil.dump(level, "MulOrDivExpr");
        DumpUtil.dump(level + 1, "operator: " + mulOrDivExpr.operator.name());
        DumpUtil.dump(level + 1, "left expr");
        mulOrDivExpr.leftExpr.dump(level + 2);
        DumpUtil.dump(level + 1, "right expr");
        mulOrDivExpr.rightExpr.dump(level + 2);
    }

    public static void dump(int level, ParenthesisExpr expr) {
        DumpUtil.dump(level, "ParenthesisExpr");
        expr.expr.dump(level + 1);
    }

    public static void dump(int level, PrefixUnaryExpr prefixUnaryExpr) {
        DumpUtil.dump(level, "Prefix Unary Expr");
        DumpUtil.dump(level+1, "operator: "+ prefixUnaryExpr.operator.type.name());
        DumpUtil.dump(level+1, "expr");
        prefixUnaryExpr.expr.dump(level+2);
    }

    public static void dump(int level, PrimaryExpr expr) {
        DumpUtil.dump(level, "primary: " + expr.token.type + ", " + expr.token.value);
    }

    public static void dump(int level, RelationExpr relationExpr) {
        DumpUtil.dump(level, "RelationExpr operator: "+ relationExpr.operator.name());
        DumpUtil.dump(level, "left Expr");
        relationExpr.leftExpr.dump(level+1);
        DumpUtil.dump(level, "right Expr");
        relationExpr.rightExpr.dump(level+1);
    }

    public static void dump(int level, ShiftExpr shiftExpr) {
        DumpUtil.dump(level, "Shift Expr");
        DumpUtil.dump(level+1, "operator: "+ shiftExpr.operator.name());
        DumpUtil.dump(level+1, "left Expr");
        shiftExpr.leftExpr.dump(level+2);
        DumpUtil.dump(level+1, "right Expr");
        shiftExpr.rightExpr.dump(level+2);
    }

    public static void dump(int level, StructAssignExpr structAssignExpr) {
        DumpUtil.dump(level, "Struct Assign Expr");
        for (int i = 0; i < structAssignExpr.fieldValues.size(); i++) {
            DumpUtil.dump(level + 1, "fields[" + i + "]: " + structAssignExpr.fieldValues.get(i).nameToken.name);
            DumpUtil.dump(level + 1, "expr");
            structAssignExpr.fieldValues.get(i).expr.dump(level + 2);
        }
    }

    public static void dump(int level, SuffixUnaryExpr suffixUnaryExpr) {
        DumpUtil.dump(level, "SuffixUnaryExpr: " + suffixUnaryExpr.operator.name());
        suffixUnaryExpr.expr.dump(level + 1);
    }

    public static void dump(int level, BreakStmtNode breakStmtNode) {
        DumpUtil.dump(level, "BREAK");
    }

    public static void dump(int level, ContinueStmtNode continueStmtNode) {
        DumpUtil.dump(level, "continue");
    }

    public static void dump(int level, ExprStmtNode exprStmtNode) {
        DumpUtil.dump(level, "ExprStmt");
        exprStmtNode.expr.dump(level +1);
    }

    public static void dump(int level, ForStmtNode forStmtNode) {
        DumpUtil.dump(level, "For stmt");
        DumpUtil.dump(level + 1, "init expr");
        if (forStmtNode.init != null) {
            forStmtNode.init.dump(level + 2);
        }
        DumpUtil.dump(level + 1, "condition");
        if (forStmtNode.condition != null) {
            forStmtNode.condition.dump(level + 2);
        }
        DumpUtil.dump(level + 1, "operation");
        if (forStmtNode.operation != null) {
            forStmtNode.operation.dump(level + 2);
        }
        DumpUtil.dump(level + 1, "stmt");
        forStmtNode.stmt.dump(level + 2);
    }

    public static void dump(int level, IfStmtNode ifStmtNode) {
        DumpUtil.dump(level, "IfStmtNode:");
        DumpUtil.dump(level + 1, "condition:");
        ifStmtNode.condition.dump(level + 2);
        DumpUtil.dump(level + 1, "if stmt:");
        ifStmtNode.ifStmt.dump(level + 2);
        ifStmtNode.elseIfStmts.forEach(i -> {
            DumpUtil.dump(level + 1, "else if stmt:");
            DumpUtil.dump(level + 2, "condition:");
            i.condition.dump(level + 3);
            DumpUtil.dump(level + 2, "stmt:");
            i.stmt.dump(level + 3);
        });
        if (ifStmtNode.elseStmt != null) {
            DumpUtil.dump(level + 1, "else stmt:");
            ifStmtNode.elseStmt.dump(level + 2);
        }
    }

    public static void dump(int level, ReturnStmtNode returnStmtNode) {
        DumpUtil.dump(level, "return");
        if (returnStmtNode.expr != null) {
            returnStmtNode.expr.dump(level + 1);
        }
    }

    public static void dump(int level, SwitchStmtNode switchStmtNode) {
        // todo
        DumpUtil.dump(level);
        dump("\n");
    }

    public static void dump(int level, VarDefStmtNode varDefStmtNode) {
        DumpUtil.dump(level, "var defs:");
        varDefStmtNode.varDefNodes.forEach(i -> dump(level + 1, i));
    }

    public static void dump(int level, WhileStmtNode whileStmtNode) {
        DumpUtil.dump(level, "While stmt");
        DumpUtil.dump(level + 1, "condition");
        whileStmtNode.condition.dump(level + 2);
        DumpUtil.dump(level + 1, "stmt");
        whileStmtNode.stmt.dump(level + 2);
    }

    public static void dump(int level, EmptyStmtNode emptyStmtNode) {
        dump(level, "empty");
    }
}
