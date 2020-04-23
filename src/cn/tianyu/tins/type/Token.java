package cn.tianyu.tins.type;

/**
 * 词法单元
 */
public class Token {
    public Type type;
    public String name;
    public Object value;

    public enum Type {
        IDENTIFIER,
        NUMBER_VAL,
        STRING_VAL,
        // 关键字
        INT,
        CHAR,
        BOOL,
        STRING,
        FLOAT,
        DOUBLE,
        VOID,
        STRUCT,
        RETURN,
        WHILE,
        FOR,
        IF,
        ELSE,
        MAIN,
        IMPORT,
        THIS,
        // 基本运算
        ADD,
        SUB,
        MUL,
        DIV,
        MOD,
        // 逻辑运算
        GT,
        LT,
        GE,
        LE,
        EQ,
        NE,
        OR,
        AND,
        NOT,
        // 位运算
        BOR,
        BAND,
        BXOR,
        // 自增，自减，左移右移
        INC,
        DEC,
        LSH,
        RSH,
        ASSIGN,
        // 左右花括号, 左右圆括号(parenthesis)，左右方括号，点，分号，逗号，单双引号
        LBRACE,
        RBRACE,
        LPARENT,
        RPARENT,
        LBRACKET,
        RBRACKET,
        DOT,
        SEMICOLON,
        COMMA,
        SINGLE_QUOTATION,
        DOUBLE_QUOTATION,
        NEWLINE,
        // 结束符
        END
    }
}
