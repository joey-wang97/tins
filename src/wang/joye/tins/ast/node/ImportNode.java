package wang.joye.tins.ast.node;

import wang.joye.tins.util.DumpUtil;
import wang.joye.tins.visitor.ASTVisitor;

import java.util.LinkedList;
import java.util.List;

/**
 * 顶层结点
 */
public class ImportNode extends Node {

    public List<String> folders = new LinkedList<>();

    @Override
    public void dump(int level) {
        DumpUtil.dump(level);
        folders.forEach(i -> System.out.print(i + " "));
        System.out.println();
    }

    @Override
    public void check(ASTVisitor visitor) {
        visitor.visit(this);
    }
}
