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
            if (i.varNameToken.name.equals(varNode.varNameToken.name)) {
                throw new RuntimeException("at line " + varNode.varNameToken.line + " :duplicated def: " + varNode.varNameToken);
            }
        });
        varList.add(varNode);
    }

    public VarDefNode get(String varName) {
        for (VarDefNode varDefNode : varList) {
            if (varDefNode.varNameToken.name.equals(varName))
                return varDefNode;
        }
        if (parent != null)
            return parent.get(varName);
        return null;
    }
}
