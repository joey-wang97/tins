package cn.tianyu.tins;

import cn.tianyu.tins.type.Token;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

public class Lexer {

    String src;
    int position = 0;
    int line = 1, col = 0;
    List<Token> list = new LinkedList<>();

    public Lexer(String fileName) {
        readFile(fileName);
    }

    /**
     * 直接读取文件内容到src
     */
    private void readFile(String fileName) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader(fileName));
            StringBuilder builder = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                // readLine函数不读入换行符，需要自己添加
                builder.append(line).append('\n');
            }
            // 减去最后一个换行符
            builder.deleteCharAt(builder.length() - 1);
            src = builder.toString();
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(-1);
        }
    }

    /**
     * 辅助函数
     * 当读取到文件尾的时候，返回0，此时不会继续任何循环
     */
    private char charAt(int position) {
        if (position < src.length())
            return src.charAt(position);
        else
            return 0;
    }

    public void unexpectedToken(Token.Type unexpectedType) {
        error("unexpected token type: " + unexpectedType.name());
    }

    public void unexpectedToken(Token.Type unexpectedType, Token.Type expectedType) {
        error("unexpected token type: " + unexpectedType.name() + ", expected is: " + expectedType.name());
    }

    public void error(String str) {
        System.err.println(String.format("at line %d:%d, %s", line, col, str));
        System.exit(-1);
    }

    public Token peek() {
        if (list.isEmpty()) {
            list.add(next());
        }
        return list.get(0);
    }

    public void back(Token token) {
        list.add(0, token);
    }

    public Token match(Token.Type type) {
        Token token = next();
        if (token.type != type) {
            unexpectedToken(token.type, type);
        }
        return token;
    }

    public Token next() {
        if (!list.isEmpty()) {
            return list.remove(0);
        }

        Token token = new Token();
        if (position >= src.length()) {
            token.type = Token.Type.END;
            return token;
        }

        char c;
        while (((c = charAt(position++)) == ' ' || c == '\t')) {
            col++;
            if (c == '\t')
                col += 3;
        }

        // 当前正在读的位置
        int oldPosition = position - 1;

        // identifier
        if (Character.isLetter(c) || c == '_') {
            c = charAt(position);
            while (Character.isLetter(c) || Character.isDigit(c)) {
                c = charAt(++position);
            }
            String name = src.substring(oldPosition, position);
            token.type = checkTokenType(name);
            if (token.type == Token.Type.IDENTIFIER)
                token.name = name;
        } else if (Character.isDigit(c)) {
            // digit
            token.type = Token.Type.NUMBER_VAL;
            if (c != '0') {
                token.value = parseDecimal();
            } else if (src.charAt(position) == 'X' || src.charAt(position) == 'x') {
                token.value = parseHex();
            } else {
                token.value = parseOct();
            }
        } else if (c == '#') {
            skipComment();
            return next();
        } else if (c == '+') {
            token.type = Token.Type.ADD;
            if (charAt(position) == '+') {
                position++;
                token.type = Token.Type.INC;
            }
        } else if (c == '-') {
            token.type = Token.Type.SUB;
            if (charAt(position) == '-') {
                position++;
                token.type = Token.Type.DEC;
            }
        } else if (c == '*') {
            token.type = Token.Type.MUL;
        } else if (c == '/') {
            token.type = Token.Type.DIV;
        } else if (c == '%') {
            token.type = Token.Type.MOD;
        } else if (c == '>') {
            token.type = Token.Type.GT;
            if (charAt(position) == '=') {
                position++;
                token.type = Token.Type.GE;
            } else if (charAt(position) == '>') {
                token.type = Token.Type.RSH;
            }
        } else if (c == '<') {
            token.type = Token.Type.LT;
            if (charAt(position) == '=') {
                position++;
                token.type = Token.Type.LE;
            } else if (charAt(position) == '<') {
                token.type = Token.Type.LSH;
            }
        } else if (c == '=') {
            token.type = Token.Type.ASSIGN;
            if (charAt(position) == '=') {
                position++;
                token.type = Token.Type.EQ;
            }
        } else if (c == '!') {
            token.type = Token.Type.NOT;
            if (charAt(position) == '=') {
                position++;
                token.type = Token.Type.NE;
            }
        } else if (c == '|') {
            token.type = Token.Type.BOR;
            if (charAt(position) == '|') {
                position++;
                token.type = Token.Type.OR;
            }
        } else if (c == '&') {
            token.type = Token.Type.BAND;
            if (charAt(position) == '&') {
                position++;
                token.type = Token.Type.AND;
            }
        } else if (c == '^') {
            token.type = Token.Type.BXOR;
        } else if (c == '{') {
            token.type = Token.Type.LBRACE;
        } else if (c == '}') {
            token.type = Token.Type.RBRACE;
        } else if (c == '[') {
            token.type = Token.Type.LBRACKET;
        } else if (c == ']') {
            token.type = Token.Type.RBRACKET;
        } else if (c == '(') {
            token.type = Token.Type.LPARENT;
        } else if (c == ')') {
            token.type = Token.Type.RPARENT;
        } else if (c == ';') {
            token.type = Token.Type.SEMICOLON;
        } else if (c == ',') {
            token.type = Token.Type.COMMA;
        } else if (c == '.') {
            token.type = Token.Type.DOT;
        } else if (c == '"') {
            token.type = Token.Type.DOUBLE_QUOTATION;
        } else if (c == '\'') {
            token.type = Token.Type.SINGLE_QUOTATION;
        } else if (c == '\n') {
            token.type = Token.Type.NEWLINE;
            // 遇到换行符时，oldPosition还停留在换行符，计算col时，会将换行符也算上，所以此处往后移动一个字符
            oldPosition++;
            line++;
            col = 0;
        } else {
            System.err.println(String.format("lex error at line %d:%d, unexpected char: %c(%d)", line, col, charAt(oldPosition), (int) charAt(oldPosition)));
            System.exit(-1);
        }
        col += position - oldPosition + 1;
        return token;
    }

    /**
     * 转化十进制
     */
    private int parseDecimal() {
        int sum = charAt(position - 1) - '0';
        char c = charAt(position);
        while (c >= '0' && c <= '9') {
            sum = sum * 10 + c - '0';
            c = charAt(++position);
        }
        return sum;
    }

    private int parseHex() {
        char c = charAt(++position);
        int sum = 0;
        while (c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f') {
            int r = c - '0';
            if (c >= 'A')
                r = c - 'A';
            if (c >= 'a')
                r = c - 'a';
            sum = sum * 16 + r;
            c = charAt(++position);
        }
        return sum;
    }

    private int parseOct() {
        char c = charAt(position);
        int sum = c - '0';
        while (c >= '0' && c <= '7') {
            sum = sum * 8 + c - '0';
            c = charAt(++position);
        }
        return sum;
    }

    /**
     * skip comment
     */
    private void skipComment() {
        while (position < src.length() && charAt(position) != '\n')
            position++;
    }

    /**
     * 得到一个标识符的类型，是标识符还是关键字
     */
    private Token.Type checkTokenType(String identifier) {
        int start = Token.Type.INT.ordinal();
        int end = Token.Type.THIS.ordinal();
        for (int i = start; i <= end; i++) {
            String keyword = Token.Type.values()[i].name().toLowerCase();
            if (keyword.equals(identifier))
                return Token.Type.values()[i];
        }
        return Token.Type.IDENTIFIER;
    }

}
