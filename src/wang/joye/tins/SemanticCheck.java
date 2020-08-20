package wang.joye.tins;

import wang.joye.tins.ast.AST;
import wang.joye.tins.ast.expr.ArrExpr;
import wang.joye.tins.ast.expr.StructAssignExpr;
import wang.joye.tins.ast.node.*;
import wang.joye.tins.ast.stmt.*;
import wang.joye.tins.type.Scope;
import wang.joye.tins.type.Token;
import wang.joye.tins.util.ErrorUtil;

import java.util.ArrayList;
import java.util.List;

public class SemanticCheck {

    public static AST globalAst = null;

    // 所有struct的名字
    private static List<String> structNames = new ArrayList<>();
    private static Scope topScope = new Scope();

    public static void check(AST ast) {
        // 保存ast，结构体检查时需要获取其中的struct定义
        globalAst = ast;
        // TODO check import
        checkStructDef(ast.structDefNodes);
        checkVarDefs(ast.varDefNodes);
        checkFuncDefs(ast.funcDefNodes);
    }

    private static void checkStructDef(List<StructDefNode> structDefs) {
        // 检查结构体是否同名
        for (StructDefNode structDef : structDefs) {
            if (structNames.contains(structDef.nameToken.name)) {
                ErrorUtil.error(structDef.nameToken.line, "duplicated struct name: " + structDef.nameToken.name);
            }
            structNames.add(structDef.nameToken.name);
            checkVarDefs(structDef.fieldDefs);
        }
    }

    private static void checkVarDefs(List<VarDefNode> varDefs) {
        List<String> varNames = new ArrayList<>(varDefs.size());
        // 检查是否存在同名变量
        for (VarDefNode var : varDefs) {
            if (varNames.contains(var.varNameToken.name)) {
                ErrorUtil.error(var.varNameToken.line, "duplicated var define: " + var.varNameToken.name);
            }
            varNames.add(var.varNameToken.name);
            checkVarDef(var);
        }
    }

    private static void checkFuncDefs(List<FuncDefNode> funDefs) {
        List<FuncDefNode> list = new ArrayList<>();
        // 检查函数是否合法（函数名一样时，可重载，不可覆盖）
        for (int i = 0; i < funDefs.size(); i++) {
            for (int j = i + 1; j < funDefs.size(); j++) {
                checkSameFunc(funDefs.get(i), funDefs.get(j));
            }
            checkFuncDef(funDefs.get(i));
        }
    }

    private static void checkSameFunc(FuncDefNode func1, FuncDefNode func2) {
        // 函数名不同
        if (!func1.funcNameToken.name.equals(func2.funcNameToken.name))
            return;
        // 函数参数不同
        if (func1.paramNode.size() != func2.paramNode.size())
            return;
        // 函数名和函数参数数量皆相同，检查参数类型
        for (int i = 0; i < func1.paramNode.size(); i++) {
            FuncParamNode func1Param = func1.paramNode.get(i);
            FuncParamNode func2Param = func2.paramNode.get(i);
            // 数组维度不同
            if (func1Param.dimensionLength != func2Param.dimensionLength)
                return;
            // 可变数组
            if (func1Param.variableArr != func2Param.variableArr)
                return;
            // 参数类型不一致
            if (func1Param.paramTypeToken.type != func2Param.paramTypeToken.type)
                return;
            // 如果参数都是自定义类型，但是类型名不一样
            // func1(Dog dog)
            // func2(Cat cat)
            if (func1Param.paramTypeToken.type == Token.Type.IDENTIFIER
                    && !func1Param.paramTypeToken.name.equals(func2Param.paramTypeToken.name))
                return;
        }
        // 以上条件都不满足，证明函数在各个方面都一致，即重复定义了函数
        ErrorUtil.error(func2.funcNameToken.line, "duplicated func define: " + func2.funcNameToken.name);
    }

    private static void checkVarDef(VarDefNode varDef) {
        // 如果变量为数组
        if (varDef.eachDimensionLength.size() > 0) {
            checkArrDef(varDef);
        }
        // 如果变量为结构体(自定义类型)
        if (varDef.varTypeToken.type == Token.Type.IDENTIFIER) {
            checkStructDef(varDef);
        }
        // 检查左值和右值是否匹配
        ExprUtil.checkMatch(varDef.varTypeToken, varDef.value);
    }

    private static void checkArrDef(VarDefNode varDef) {
        // 如果数组未初始化赋值，必须初始化维度
        if (varDef.value == null) {
            for (int i = 0; i < varDef.eachDimensionLength.size(); i++) {
                if (varDef.eachDimensionLength.get(i) == null) {
                    ErrorUtil.error(varDef.varNameToken.line, "array must initialize dimension");
                }
            }
            return;
        }
        // value如果不为空，必须为数组表达式
        if (!(varDef.value instanceof ArrExpr)) {
            ErrorUtil.error(varDef.varNameToken.line, "var type is array, var value is " + varDef.value.getClass());
        }

        // 检查数组定义维度和表达式维度是否一致
        int dimension = 1;
        ArrExpr arrExpr = (ArrExpr) varDef.value;
        while (arrExpr.exprs.get(0) instanceof ArrExpr) {
            dimension++;
            arrExpr = (ArrExpr) arrExpr.exprs.get(0);
        }
        if (dimension != varDef.eachDimensionLength.size()) {
            ErrorUtil.error(varDef.varNameToken.line, "array define and array value has different dimension");
        }

        // 检查数组中每个元素，类型是否正确
        for (ExprNode arrElement : arrExpr.exprs) {
            ExprUtil.checkMatch(varDef.varTypeToken, arrElement);
        }
    }

    private static void checkStructDef(VarDefNode varDef) {
        if (!(varDef.value instanceof StructAssignExpr)) {
            ErrorUtil.error(varDef.varNameToken.line, varDef.value.getClass() + "cannot assign to " + varDef.varTypeToken.name);
        }
        StructAssignExpr structAssignExpr = (StructAssignExpr) varDef.value;

        // 从AST中查找结构体的定义
        StructDefNode structDef = null;
        for (StructDefNode i : globalAst.structDefNodes) {
            if (i.nameToken.name.equals(varDef.varTypeToken.name)) {
                structDef = i;
                break;
            }
        }
        // 如果没有找到对应结构体，则抛出错误
        if (structDef == null) {
            ErrorUtil.error(varDef.varTypeToken.line, "cannot find struct define: " + varDef.varNameToken.name);
        }

        if (structAssignExpr.fieldValues.size() > structDef.fieldDefs.size()) {
            ErrorUtil.error("too many field in struct: " + structDef.nameToken.name);
        }
        // 如果赋值结构体时，每个属性前必须加属性名
        for (StructAssignExpr.ObjectField fieldValue : structAssignExpr.fieldValues) {
            // 检查是否具有属性名
            if (fieldValue.nameToken == null) {
                ErrorUtil.error(ExprUtil.getLine(fieldValue.expr), "you must specified field name in " + varDef.varNameToken.value);
            }
            VarDefNode fieldDef = structDef.getFieldDef(fieldValue.nameToken.name);

            // 检查属性定义和属性表达式，是否匹配
            ExprUtil.checkMatch(fieldDef.varTypeToken, fieldValue.expr);
        }
    }

    /**
     * 检查函数定义是否合法
     * 此处不需要检查函数重复定义的问题，在之前已经检查过
     */
    private static void checkFuncDef(FuncDefNode funcDef) {
        // 检查函数语句是否合法
        SemanticCheck.checkCompoundStmt(funcDef.bodyStmt);

        // 检查return语句和函数返回类型是否一致
        if (funcDef.funcTypeToken.type != Token.Type.VOID) {
            if (funcDef.bodyStmt.stmts.size() == 0) {
                ErrorUtil.error(funcDef.funcTypeToken.line, "func " + funcDef.funcNameToken.name + " must contain return stmt");
            }
            // 检查函数体的return语句
            if (!checkStmtReturnType(funcDef.funcTypeToken, funcDef.bodyStmt)) {
                ErrorUtil.error(funcDef.funcTypeToken.line, "func " + funcDef.funcNameToken.name + " must contain return stmt");
            }
        }
    }

    private static void checkCompoundStmt(CompoundStmtNode compoundStmt) {
        // TODO 是使用visitor模式？还是?
    }


    private static void checkExpr(ExprNode expr) {

    }

    /**
     * 检查 stmt是否return语句，或者是否包含return语句, 如:
     * <pre>
     *  if(condition)
     *      return a;
     *  else
     *      return b;
     * </pre>
     * 先检查是否为return，再检查return的类型是否与期望类型一致.
     */
    private static boolean checkStmtReturnType(Token type, StmtNode stmt) {
        if (stmt instanceof ReturnStmtNode) {
            ExprUtil.checkMatch(type, ((ReturnStmtNode) stmt).expr);
        }
        if (stmt instanceof CompoundStmtNode) {
            // 取出函数体的最后一条语句
            CompoundStmtNode compoundStmt = (CompoundStmtNode) stmt;
            if (compoundStmt.stmts.size() == 0) {
                return false;
            }
            StmtNode lastStmt = compoundStmt.stmts.get(compoundStmt.stmts.size() - 1);
            return checkStmtReturnType(type, lastStmt);
        }
        if (stmt instanceof IfStmtNode) {
            IfStmtNode ifStmtNode = (IfStmtNode) stmt;
            if (!checkStmtReturnType(type, ifStmtNode.ifStmt))
                return false;
            for (IfStmtNode.ElseIfStmt elseIfStmt : ifStmtNode.elseIfStmts) {
                if (!checkStmtReturnType(type, elseIfStmt.stmt))
                    return false;
            }
            if (ifStmtNode.elseStmt != null && !checkStmtReturnType(type, ifStmtNode.elseStmt))
                return false;
            return true;
        }
        if (stmt instanceof ForStmtNode) {
            ForStmtNode forStmtNode = (ForStmtNode) stmt;
            return checkStmtReturnType(type, forStmtNode.stmt);
        }
        if (stmt instanceof WhileStmtNode) {
            WhileStmtNode whileStmt = (WhileStmtNode) stmt;
            return checkStmtReturnType(type, whileStmt.stmt);
        }

        return false;
    }
}
