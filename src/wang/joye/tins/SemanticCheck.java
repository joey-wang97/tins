package wang.joye.tins;

import wang.joye.tins.ast.AST;
import wang.joye.tins.ast.expr.ArrExpr;
import wang.joye.tins.ast.node.*;
import wang.joye.tins.type.Scope;
import wang.joye.tins.type.Token;
import wang.joye.tins.util.ErrorUtil;

import java.util.ArrayList;
import java.util.List;

public class SemanticCheck {

    // 所有struct的名字
    private static List<String> structNames = new ArrayList<>();
    private static Scope topScope = new Scope();

    public static void check(AST ast) {
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
            checkVarDefs(structDef.fields);
        }
    }

    private static void checkVarDefs(List<VarDefNode> varDefs) {
        List<String> varNames = new ArrayList<>(varDefs.size());
        // 检查是否存在同名变量
        for (VarDefNode var : varDefs) {
            if (varNames.contains(var.varName.name)) {
                ErrorUtil.error(var.varName.line, "duplicated var define: " + var.varName.name);
            }
            varNames.add(var.varName.name);
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
        if (!func1.funcName.name.equals(func2.funcName.name))
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
            if (func1Param.paramType.type != func2Param.paramType.type)
                return;
            // 如果参数都是自定义类型，但是类型名不一样
            // func1(Dog dog)
            // func2(Cat cat)
            if (func1Param.paramType.type == Token.Type.IDENTIFIER
                    && !func1Param.paramType.name.equals(func2Param.paramType.name))
                return;
        }
        // 以上条件都不满足，证明函数在各个方面都一致，即重复定义了函数
        ErrorUtil.error(func2.funcName.line, "duplicated func define: " + func2.funcName.name);
    }

    private static void checkVarDef(VarDefNode varDef) {
        // 如果变量为数组类型
        if (varDef.eachDimensionLength.size() > 0) {
            checkArrDef(varDef);
        }
        // TODO 检查左值和右值类型
    }

    private static void checkArrDef(VarDefNode varDef) {
        // 如果数组未初始化赋值，必须初始化维度
        if (varDef.value == null) {
            for (int i = 0; i < varDef.eachDimensionLength.size(); i++) {
                if (varDef.eachDimensionLength.get(i) == null) {
                    ErrorUtil.error(varDef.varName.line, "array must initialize dimension");
                }
            }
            return;
        }
        // value如果不为空，必须为数组表达式
        if (!(varDef.value instanceof ArrExpr)) {
            ErrorUtil.error(varDef.varName.line, "var type is array, var value is " + varDef.value.getClass());
        }

        // 检查数组定义维度和表达式维度是否一致
        int dimension = 1;
        ArrExpr value = (ArrExpr) varDef.value;
        while (value.exprs.get(0) instanceof ArrExpr) {
            dimension++;
            value = (ArrExpr) value.exprs.get(0);
        }
        if (dimension != varDef.eachDimensionLength.size()) {
            ErrorUtil.error(varDef.varName.line, "array define and array value has different dimension");
        }
    }

    private static void checkFuncDef(FuncDefNode funcDef) {

    }

    private static void checkExpr(ExprNode expr) {

    }
}
