package wang.joye.tins.ast.node;

import wang.joye.tins.type.Token;
import wang.joye.tins.util.DumpUtil;

import java.util.ArrayList;
import java.util.List;

/**
 * 结构体
 */
public class StructDefNode extends Node {
    public Token nameToken;
    public List<VarDefNode> fields = new ArrayList<>();

    @Override
    public void dump(int level) {
        DumpUtil.dump(level, "struct name: " + nameToken);
        DumpUtil.dump(level, "struct fields:");
        fields.forEach(i -> i.dump(level + 1));
        System.out.println();
    }
}
