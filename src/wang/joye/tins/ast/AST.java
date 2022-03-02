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
}
