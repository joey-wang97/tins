package wang.joye.tins.ast.node;

/**
 * 顶层结点
 */
public class ExprNode extends Node{

    @Override
    public void dump(int level) {
        super.printIndent(level);
        System.out.println("it's just expr node");
    }
}
