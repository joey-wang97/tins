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
        CHAR_VAL,
        DOUBLE_VAL,
        FLOAT_VAL,
        // 关键字
        INT,
        CHAR,
        BOOL,
        STRING,
        FLOAT,
        DOUBLE,
        VOID,
        STRUCT,
        FUNC,
        RETURN,
        WHILE,
        FOR,
        IF,
        ELSE,
        SWITCH,
        CASE,
        DEFAULT,
        BREAK,
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
        B_OR,
        B_AND,
        B_XOR,
        // 自增，自减，左移右移
        INC,
        DEC,
        LSH,
        RSH,
        // 各种形式的赋值 = += -= *= /= %= &= |= ^= <<= >>=
        ASSIGN, ADD_ASSIGN, SUB_ASSIGN, MUL_ASSIGN, DIV_ASSIGN, MOD_ASSIGN, B_AND_ASSIGN, B_OR_ASSIGN, B_XOR_ASSIGN, LSH_ASSIGN, RSH_ASSIGN,
        // 左右花括号, ，左右方括号，点，分号，逗号，单双引号
        L_CURLY_BRACKET,
        R_CURLY_BRACKET,
        // 左右圆括号
        OPEN_PARENTHESIS,
        CLOSE_PARENTHESIS,
        // 左右方括号
        L_SQUARE_BRACKET,
        R_SQUARE_BRACKET,
        DOT,
        // 分号
        SEMICOLON,
        // 波浪号
        TILDE,
        // 冒号
        COLON,
        // 问号
        QUESTION_MARK,
        // 按位取反
        BIT_REVERSE,
        COMMA,
        // 单双引号
        SINGLE_QUOTATION,
        DOUBLE_QUOTATION,
        LINE_BREAK,
        // 结束符
        END
    }
}
