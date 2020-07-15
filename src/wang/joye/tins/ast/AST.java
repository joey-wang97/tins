package wang.joye.tins.ast;

import wang.joye.tins.ast.node.FuncDefNode;
import wang.joye.tins.ast.node.ImportNode;
import wang.joye.tins.ast.node.StructDefNode;
import wang.joye.tins.ast.node.VarDefNode;

import java.util.ArrayList;
import java.util.List;

/**
 * 顶层AST
 */
public class AST {
    public List<ImportNode> importNodes = new ArrayList<>();
    public List<StructDefNode> structDefNodes = new ArrayList<>();
    public List<VarDefNode> varDefNodes = new ArrayList<>();
    public List<FuncDefNode> funcDefNodes = new ArrayList<>();

    public void dump() {
        System.out.println("import list");
        importNodes.forEach(i -> i.dump(1));
        System.out.println("struct def list");
        structDefNodes.forEach(i -> i.dump(1));
        System.out.println("var def list");
        varDefNodes.forEach(i -> i.dump(1));
        System.out.println("func def list");
        funcDefNodes.forEach(i -> i.dump(1));
    }
}
