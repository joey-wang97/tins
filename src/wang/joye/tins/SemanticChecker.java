package wang.joye.tins;

import wang.joye.tins.ast.AST;
import wang.joye.tins.ast.expr.*;
import wang.joye.tins.ast.node.*;
import wang.joye.tins.ast.stmt.*;
import wang.joye.tins.type.ExprType;
import wang.joye.tins.type.Scope;
import wang.joye.tins.type.Token;
import wang.joye.tins.util.ErrorUtil;
import wang.joye.tins.visitor.ASTVisitor;
import wang.joye.tins.visitor.ExprTypeVisitor;

import java.util.ArrayList;
import java.util.List;

public class SemanticChecker implements ASTVisitor {

    public static AST globalAst = null;

    /**
     * 所有struct的名字
     */
    private static final List<String> STRUCT_NAMES = new ArrayList<>();
    public static Scope currScope = new Scope();

    public void check(AST ast) {
        // 保存ast，结构体检查时需要获取其中的struct定义
        globalAst = ast;
        ast.importNodes.forEach(this::visit);
        checkStructDef(ast.structDefNodes);
        checkVarDefs(ast.varDefNodes);
        checkFuncDefs(ast.funcDefNodes);
    }

    private void checkStructDef(List<StructDefNode> structDefs) {
        // 检查结构体是否同名
        for (StructDefNode structDef : structDefs) {
            if (STRUCT_NAMES.contains(structDef.nameToken.name)) {
                ErrorUtil.error(structDef.nameToken.line, "duplicated struct name: " + structDef.nameToken.name);
            }
            STRUCT_NAMES.add(structDef.nameToken.name);
            checkVarDefs(structDef.fieldDefs);
        }
    }

    private void checkVarDefs(List<VarDefNode> varDefs) {
        // 保存变量到作用域中
        currScope.varList = varDefs;
        List<String> varNames = new ArrayList<>(varDefs.size());
        // 检查是否存在同名变量
        for (VarDefNode var : varDefs) {
            if (varNames.contains(var.varNameToken.name)) {
                ErrorUtil.error(var.varNameToken.line, "duplicated var define: " + var.varNameToken.name);
            }
            varNames.add(var.varNameToken.name);
            visit(var);
        }
    }

    private void checkFuncDefs(List<FuncDefNode> funDefs) {
        List<FuncDefNode> list = new ArrayList<>();
        // 检查函数是否合法（函数名一样时，可重载，不可覆盖）
        for (int i = 0; i < funDefs.size(); i++) {
            for (int j = i + 1; j < funDefs.size(); j++) {
                checkSameFunc(funDefs.get(i), funDefs.get(j));
            }
            visit(funDefs.get(i));
        }
    }

    private void checkSameFunc(FuncDefNode func1, FuncDefNode func2) {
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

    @Override
    public void visit(VarDefNode varDef) {
        // 如果变量为数组
        if (varDef.eachDimensionLength.size() > 0) {
            checkArrDef(varDef);
        }
        // 如果变量为结构体(自定义类型)
        if (varDef.varTypeToken.type == Token.Type.IDENTIFIER) {
            checkStructDef(varDef);
        }
        // 检查左值和右值是否匹配
        if (varDef.value != null) {
            checkMatch(varDef.varTypeToken, varDef.value, varDef.eachDimensionLength.size() > 0);
        }
    }

    private void checkArrDef(VarDefNode varDef) {
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
            checkMatch(varDef.varTypeToken, arrElement, varDef.eachDimensionLength.size() > 0);
        }
    }

    private void checkStructDef(VarDefNode varDef) {
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
                ErrorUtil.error(fieldValue.expr.getLine(), "you must specified field name in " + varDef.varNameToken.value);
            }
            VarDefNode fieldDef = structDef.getFieldDef(fieldValue.nameToken.name);

            // 检查属性定义和属性表达式，是否匹配
            checkMatch(fieldDef.varTypeToken, fieldValue.expr, fieldDef.eachDimensionLength.size() > 0);
        }
    }

    /**
     * 检查函数定义是否合法
     * 此处不需要检查函数重复定义的问题，在之前已经检查过
     */
    @Override
    public void visit(FuncDefNode funcDef) {
        // 检查函数语句是否合法
        visit(funcDef.bodyStmt);

        // 检查return语句和函数返回类型是否一致
        if (funcDef.funcTypeToken.type != Token.Type.VOID) {
            if (funcDef.bodyStmt.stmts.size() == 0) {
                ErrorUtil.error(funcDef.funcTypeToken.line, "func " + funcDef.funcNameToken.name + " must contain return stmt");
            }
            // 检查函数体的return语句
            if (!checkStmtReturnType(funcDef.funcTypeToken, funcDef.bodyStmt, funcDef.eachDimensionLength.size() > 0)) {
                ErrorUtil.error(funcDef.funcTypeToken.line, "func " + funcDef.funcNameToken.name + " must contain return stmt");
            }
        }
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
    private static boolean checkStmtReturnType(Token type, StmtNode stmt, boolean isArr) {
        if (stmt instanceof ReturnStmtNode) {
            checkMatch(type, ((ReturnStmtNode) stmt).expr, isArr);
        }
        if (stmt instanceof CompoundStmtNode) {
            // 取出函数体的最后一条语句
            CompoundStmtNode compoundStmt = (CompoundStmtNode) stmt;
            if (compoundStmt.stmts.size() == 0) {
                return false;
            }
            StmtNode lastStmt = compoundStmt.stmts.get(compoundStmt.stmts.size() - 1);
            return checkStmtReturnType(type, lastStmt, isArr);
        }
        if (stmt instanceof IfStmtNode) {
            IfStmtNode ifStmtNode = (IfStmtNode) stmt;
            if (!checkStmtReturnType(type, ifStmtNode.ifStmt, isArr))
                return false;
            for (IfStmtNode.ElseIfStmt elseIfStmt : ifStmtNode.elseIfStmts) {
                if (!checkStmtReturnType(type, elseIfStmt.stmt, isArr))
                    return false;
            }
            return ifStmtNode.elseStmt == null || checkStmtReturnType(type, ifStmtNode.elseStmt, isArr);
        }
        if (stmt instanceof ForStmtNode) {
            ForStmtNode forStmtNode = (ForStmtNode) stmt;
            return checkStmtReturnType(type, forStmtNode.stmt, isArr);
        }
        if (stmt instanceof WhileStmtNode) {
            WhileStmtNode whileStmt = (WhileStmtNode) stmt;
            return checkStmtReturnType(type, whileStmt.stmt, isArr);
        }

        return false;
    }

    @Override
    public void visit(ArrExpr expr) {
        ExprType firstType = expr.exprs.get(0).getType();
        for (int i = 1; i < expr.exprs.size(); i++) {
            ExprType curType = expr.exprs.get(i).getType();
            if (differentExprType(firstType, curType))
                ErrorUtil.error(expr.getLine(), "arr expr type mismatch");
        }
    }

    /**
     * 二元表达式检查
     */
    private void binaryCheck(ExprNode left, ExprNode right) {
        // 左值是否和右值匹配
        ExprType leftType = ExprTypeVisitor.getType(left);
        ExprType rightType = ExprTypeVisitor.getType(right);
        if (differentExprType(leftType, rightType)) {
            ErrorUtil.error(left.getLine(), "assign expr type mismatch");
        }
        left.check(this);
        right.check(this);
    }

    /**
     * 一个表达式必须为整型：int或long
     */
    private void mustBeInteger(ExprNode expr) {
        if (expr.getType().type != ExprType.Type.INT
                && expr.getType().type != ExprType.Type.LONG) {
            ErrorUtil.error(expr.getLine(), "expr type must be int");
        }
    }

    /**
     * 必须为基本类型: int/long/float/double
     */
    private void mustBeSimpleType(ExprNode expr) {
        if (expr.getType().arrDimension > 0
                || expr.getType().type == ExprType.Type.STRUCT
                || expr.getType().type != ExprType.Type.STRING) {
            ErrorUtil.error(expr.getLine(), "expr type must not be string or struct");
        }
    }

    @Override
    public void visit(AssignExpr expr) {
        binaryCheck(expr.leftExpr, expr.rightExpr);
    }

    @Override
    public void visit(BitAndExpr expr) {
        binaryCheck(expr.leftExpr, expr.rightExpr);
        mustBeInteger(expr.leftExpr);
        mustBeInteger(expr.rightExpr);
    }

    @Override
    public void visit(BitOrExpr expr) {
        binaryCheck(expr.leftExpr, expr.rightExpr);
        mustBeInteger(expr.leftExpr);
        mustBeInteger(expr.rightExpr);
    }

    @Override
    public void visit(BitXorExpr expr) {
        binaryCheck(expr.leftExpr, expr.rightExpr);
        mustBeInteger(expr.leftExpr);
        mustBeInteger(expr.rightExpr);
    }

    @Override
    public void visit(CastExpr castExpr) {
        // 不可强转为string或struct
        if (castExpr.castType.type == Token.Type.IDENTIFIER
                || castExpr.castType.type == Token.Type.STRING) {
            ErrorUtil.error(castExpr.getLine(), "can't cast to struct or string");
        }
    }

    @Override
    public void visit(CondExpr expr) {
        mustBeInteger(expr.cond);
    }

    @Override
    public void visit(AddOrSubExpr expr) {
        binaryCheck(expr.leftExpr, expr.rightExpr);
        mustBeSimpleType(expr.leftExpr);
        mustBeSimpleType(expr.rightExpr);
    }

    @Override
    public void visit(EqualityExpr expr) {
        binaryCheck(expr.leftExpr, expr.rightExpr);
    }

    @Override
    public void visit(FactorExpr expr) {
        if (expr == null)
            return;
        // 检查数组维度
        ExprType type = expr.expr.getType();
        if (expr.arrIndexList != null && expr.arrIndexList.size() > 0) {
            if (type.arrDimension != expr.arrIndexList.size()) {
                ErrorUtil.error(expr.getLine(), "数组维度不同");
            }
            // 检查维度是否整型
            for (ExprNode exprNode : expr.arrIndexList) {
                mustBeInteger(exprNode);
            }
        }
        // 无法确定FactorExpr的具体类型，调用子类的检查方法
        expr.expr.check(this);
        // 检查点号之后的表达式
        visit(expr.nextFactor);
    }

    @Override
    public void visit(LogicAndExpr expr) {
        binaryCheck(expr.leftExpr, expr.rightExpr);
        mustBeInteger(expr.leftExpr);
        mustBeInteger(expr.rightExpr);
    }

    @Override
    public void visit(LogicOrExpr expr) {
        binaryCheck(expr.leftExpr, expr.rightExpr);
        mustBeInteger(expr.leftExpr);
        mustBeInteger(expr.rightExpr);
    }

    @Override
    public void visit(MulOrDivExpr expr) {
        binaryCheck(expr.leftExpr, expr.rightExpr);
        mustBeSimpleType(expr.leftExpr);
        mustBeSimpleType(expr.rightExpr);
    }

    @Override
    public void visit(ParenthesisExpr expr) {
        // 只检查括号内的表达式
        expr.expr.check(this);
    }

    @Override
    public void visit(PrefixUnaryExpr prefixUnaryExpr) {
        mustBeSimpleType(prefixUnaryExpr.expr);
    }

    @Override
    public void visit(PrimaryExpr expr) {
        // ignore 此表达式不需要检查
    }

    @Override
    public void visit(RelationExpr expr) {
        binaryCheck(expr.leftExpr, expr.rightExpr);
    }

    @Override
    public void visit(ShiftExpr expr) {
        binaryCheck(expr.leftExpr, expr.rightExpr);
        mustBeSimpleType(expr.leftExpr);
        mustBeSimpleType(expr.rightExpr);
    }

    @Override
    public void visit(StructAssignExpr expr) {

    }

    @Override
    public void visit(SuffixUnaryExpr expr) {
        mustBeSimpleType(expr.expr);
    }

    @Override
    public void visit(BreakStmtNode stmt) {

    }

    @Override
    public void visit(CaseStmtNode stmt) {

    }

    @Override
    public void visit(CompoundStmtNode compoundStmt) {
        // 进入复合语句，添加新一级作用域
        Scope tempScope = new Scope();
        tempScope.parent = currScope;
        currScope = tempScope;
        for (StmtNode stmt : compoundStmt.stmts) {
            stmt.check(this);
        }
    }

    @Override
    public void visit(ContinueStmtNode stmt) {

    }

    @Override
    public void visit(EmptyStmtNode stmt) {

    }

    @Override
    public void visit(ExprStmtNode stmt) {

    }

    @Override
    public void visit(ForStmtNode stmt) {

    }

    @Override
    public void visit(IfStmtNode stmt) {

    }

    @Override
    public void visit(ReturnStmtNode stmt) {

    }

    @Override
    public void visit(SwitchStmtNode stmt) {

    }

    @Override
    public void visit(VarDefStmtNode stmt) {

    }

    @Override
    public void visit(WhileStmtNode stmt) {

    }

    @Override
    public void visit(ImportNode importNode) {

    }

    @Override
    public void visit(StructDefNode structDefNode) {

    }

    @Override
    public void visit(FuncParamNode funcParamNode) {

    }

    /**
     * 检查token和expr的类型是否一致
     * 暂不支持隐式转换
     * TODO 暂时只支持一维数组
     */
    public static void checkMatch(Token token, ExprNode exprNode, boolean isArr) {
        ExprType exprType = exprNode.getType();
        switch (exprType.type) {
            case INT:
                if (token.type != Token.Type.INT) {
                    ErrorUtil.error(token.line, "type mismatch");
                }
                break;
            case CHAR:
                if (token.type != Token.Type.CHAR) {
                    ErrorUtil.error(token.line, "type mismatch");
                }
                break;
            case LONG:
                ErrorUtil.error(token.line, "unexpected long type!");
                break;
            case FLOAT:
                if (token.type != Token.Type.FLOAT) {
                    ErrorUtil.error(token.line, "type mismatch");
                }
                break;
            case DOUBLE:
                if (token.type != Token.Type.DOUBLE) {
                    ErrorUtil.error(token.line, "type mismatch");
                }
                break;
            case STRING:
                if (token.type != Token.Type.STRING) {
                    ErrorUtil.error(token.line, "type mismatch");
                }
                break;
            case STRUCT:
                if (token.type != Token.Type.IDENTIFIER
                        && !exprType.structName.equals(token.name)) {
                    ErrorUtil.error(token.line, "type mismatch");
                }
                break;
            default:
                ErrorUtil.error(exprNode.getLine(), "未知类型:" + exprType.type.name());
                break;
        }
        // 判断两个是不是都是数组类型，或者都不是，所以用异或
        if (isArr ^ exprNode.getType().arrDimension > 0) {
            ErrorUtil.error(token.line, "arr type mismatch");
        }
    }

    /**
     * 判断两个表达式类型 是否不同类型
     * 暂不考虑隐式转换，隐式转换需要考虑是左转右还是右转左还是都ok
     */
    public static boolean differentExprType(ExprType type1, ExprType type2) {
        // 如果都是基本类型
        if (type1.type == type2.type && type1.type != ExprType.Type.STRUCT) {
            return false;
        }
        // 如果是同一结构体类型
        return type1.type != ExprType.Type.STRUCT
                || type2.type != ExprType.Type.STRUCT
                || !type1.structName.equals(type2.structName);
    }

}