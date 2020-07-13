package cn.tianyu.tins.ast;

/**
 * 顶层结点
 */
public class ExprNode extends Node{

    @Override
    public void dump(int indent) {
        super.printIndent(indent);
        System.out.println("it's just expr node");
    }
}
