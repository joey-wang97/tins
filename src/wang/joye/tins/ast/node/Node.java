package wang.joye.tins.ast.node;

public abstract class Node {
    public int type;

    public abstract void dump(int level);
}
