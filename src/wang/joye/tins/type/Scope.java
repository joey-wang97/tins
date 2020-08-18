package wang.joye.tins.type;

import wang.joye.tins.ast.node.VarDefNode;

import java.util.List;

/**
 * 作用域
 */
public class Scope {
    public Scope parent;

    public List<VarDefNode> varList;

    public void addAndCheck(VarDefNode varNode) {
        varList.forEach(i -> {
            if (i.varName.name.equals(varNode.varName.name)) {
                throw new RuntimeException("at line " + varNode.varName.line + " :duplicated def: " + varNode.varName);
            }
        });
        varList.add(varNode);
    }
}
