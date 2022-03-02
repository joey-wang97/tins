package wang.joye.tins.ast.node;

import wang.joye.tins.visitor.ASTVisitor;

import java.util.LinkedList;
import java.util.List;

/**
 * 顶层结点
 */
public class ImportNode extends Node {

    public List<String> folders = new LinkedList<>();

    @Override
    public void check(ASTVisitor visitor) {
        visitor.visit(this);
    }
}
