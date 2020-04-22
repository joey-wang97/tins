package cn.tianyu.tins.type;

public class Node {
    int type;

    enum Type {
        ROOT,
        VAR_DECL,
        STRUCT_DECL,
        FUNC_DECL
    }
}
