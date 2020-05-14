package cn.tianyu.tins;

import cn.tianyu.tins.type.Token;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

public class Lexer {

    String src;
    /**
     * 源文件中读取的位置
     */
    int srcPosition = 0;
    int line = 1, col = 0;

    List<Token> readTokens = new LinkedList<>();
    /**
     * 下一个要获取的token位置
     */
    int tokenPosition = 0;

    public Lexer(String fileName) {
        readFile(fileName);
        // 直接一次性读取出来所有token
        Token token;
        while ((token = readNextTokenFromSrc()).type != Token.Type.END) {
            readTokens.add(token);
        }
    }

    /**
     * 使词法分析器重新从第一个token开始next
     */
    public void moveToHead() {
        tokenPosition = 0;
    }


    /**
     * 忽略换行符，取一个token
     */
    public Token peekIgnoreLineBreak() {
        while (readTokens.get(tokenPosition).type == Token.Type.LINE_BREAK) {
            tokenPosition++;
        }
        return readTokens.get(tokenPosition);
    }

    /**
     * 向前看n个非换行符token
     */
    public Token lookAheadIgnoreLineBreak(int n) {
        int p = tokenPosition;
        for (int i = 0; i < n;) {
            while (readTokens.get(p).type == Token.Type.LINE_BREAK)
                p++;
            i++;
        }
        return readTokens.get(p);
    }

    public Token match(Token.Type type) {
        Token token = readTokens.get(tokenPosition++);
        if (token.type != type) {
            unexpectedToken(token.type, type);
        }
        return token;
    }

    public Token matchIgnoreLineBreak(Token.Type type) {
        Token token;
        while ((token = readTokens.get(tokenPosition)).type == Token.Type.LINE_BREAK) {
            tokenPosition++;
        }
        if (token.type != type) {
            unexpectedToken(token.type, type);
        }
        return token;
    }

    /**
     * 返回不包括换行符的token
     * 遇到换行符则继续
     */
    public Token nextIgnoreLineBreak() {
        Token token = readTokens.get(tokenPosition);
        while (token.type == Token.Type.LINE_BREAK) {
            token = readTokens.get(++tokenPosition);
        }
        return token;
    }

    public Token next() {
        return readTokens.get(tokenPosition++);
    }

    /**
     * 直接读取整个文件内容到src
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

    /**
     * 返回所有类型的token，包括换行符
     */
    public Token readNextTokenFromSrc() {

        Token token = new Token();
        if (srcPosition >= src.length()) {
            token.type = Token.Type.END;
            return token;
        }

        char c;
        while (((c = charAt(srcPosition++)) == ' ' || c == '\t')) {
            col++;
            if (c == '\t')
                col += 3;
        }

        // 当前正在读的位置
        int oldPosition = srcPosition - 1;

        // identifier
        if (Character.isLetter(c) || c == '_') {
            c = charAt(srcPosition);
            while (Character.isLetter(c) || Character.isDigit(c)) {
                c = charAt(++srcPosition);
            }
            String name = src.substring(oldPosition, srcPosition);
            token.type = checkTokenType(name);
            if (token.type == Token.Type.IDENTIFIER)
                token.name = name;
        } else if (Character.isDigit(c)) {
            // digit
            if (c != '0') {
                // 可能为整数或小数
                token = parseDecimal();
            } else if (src.charAt(srcPosition) == 'X' || src.charAt(srcPosition) == 'x') {
                token.type = Token.Type.NUMBER_VAL;
                token.value = parseHex();
            } else {
                token.type = Token.Type.NUMBER_VAL;
                token.value = parseOct();
            }
        } else if (c == '#') {
            skipComment();
            return readNextTokenFromSrc();
        } else if (c == '+') {
            token.type = Token.Type.ADD;
            if (charAt(srcPosition) == '+') {
                srcPosition++;
                token.type = Token.Type.INC;
            } else if (charAt(srcPosition) =='=') {
                srcPosition++;
                token.type = Token.Type.ADD_ASSIGN;
            }
        } else if (c == '-') {
            token.type = Token.Type.SUB;
            if (charAt(srcPosition) == '-') {
                srcPosition++;
                token.type = Token.Type.DEC;
            } else if (charAt(srcPosition) =='=') {
                srcPosition++;
                token.type = Token.Type.SUB_ASSIGN;
            }
        } else if (c == '*') {
            token.type = Token.Type.MUL;
            if (charAt(srcPosition) == '=') {
                srcPosition++;
                token.type = Token.Type.MUL_ASSIGN;
            }
        } else if (c == '/') {
            token.type = Token.Type.DIV;
            if (charAt(srcPosition) == '=') {
                srcPosition++;
                token.type = Token.Type.DIV_ASSIGN;
            }
        } else if (c == '%') {
            token.type = Token.Type.MOD;
            if (charAt(srcPosition) == '=') {
                srcPosition++;
                token.type = Token.Type.MOD_ASSIGN;
            }
        } else if (c == '>') {
            token.type = Token.Type.GT;
            if (charAt(srcPosition) == '=') {
                srcPosition++;
                token.type = Token.Type.GE;
            } else if (charAt(srcPosition) == '>') {
                token.type = Token.Type.RSH;
            }
        } else if (c == '<') {
            token.type = Token.Type.LT;
            if (charAt(srcPosition) == '=') {
                srcPosition++;
                token.type = Token.Type.LE;
            } else if (charAt(srcPosition) == '<') {
                token.type = Token.Type.LSH;
            }
        } else if (c == '=') {
            token.type = Token.Type.ASSIGN;
            if (charAt(srcPosition) == '=') {
                srcPosition++;
                token.type = Token.Type.EQ;
            }
        } else if (c == '!') {
            token.type = Token.Type.NOT;
            if (charAt(srcPosition) == '=') {
                srcPosition++;
                token.type = Token.Type.NE;
            }
        } else if (c == '|') {
            token.type = Token.Type.B_OR;
            if (charAt(srcPosition) == '|') {
                srcPosition++;
                token.type = Token.Type.OR;
            }
        } else if (c == '&') {
            token.type = Token.Type.B_AND;
            if (charAt(srcPosition) == '&') {
                srcPosition++;
                token.type = Token.Type.AND;
            }
        } else if (c == '^') {
            token.type = Token.Type.B_XOR;
        } else if (c == '{') {
            token.type = Token.Type.L_CURLY_BRACKET;
        } else if (c == '}') {
            token.type = Token.Type.R_CURLY_BRACKET;
        } else if (c == '[') {
            token.type = Token.Type.L_SQUARE_BRACKET;
        } else if (c == ']') {
            token.type = Token.Type.R_SQUARE_BRACKET;
        } else if (c == '(') {
            token.type = Token.Type.OPEN_PARENTHESIS;
        } else if (c == ')') {
            token.type = Token.Type.CLOSE_PARENTHESIS;
        } else if (c == ';') {
            token.type = Token.Type.SEMICOLON;
        } else if (c == ',') {
            token.type = Token.Type.COMMA;
        } else if (c == '?') {
            token.type = Token.Type.QUESTION_MARK;
        }  else if (c == '~') {
            token.type = Token.Type.BIT_REVERSE;
        }  else if (c == '.') {
            token.type = Token.Type.DOT;
        } else if (c == ':') {
            token.type = Token.Type.COLON;
        } else if (c == '"') { //获取string字面量
            StringBuilder buffer = new StringBuilder();
            char v = charAt(srcPosition++);
            // todo string长度判断
            while (v != '"') {
                buffer.append(v);
                v = charAt(srcPosition++);
                if (v == 0) {
                    error("illegal string without end!");
                }
            }
            token.type = Token.Type.STRING_VAL;
            token.value = buffer.toString();
        } else if (c == '\'') {
            // todo 合法字符判断
            token.value = charAt(srcPosition++);
            token.type = Token.Type.CHAR_VAL;
            if (charAt(srcPosition++) != '\'') {
                error("illegal character1");
            }
        } else if (c == '\n') {
            token.type = Token.Type.LINE_BREAK;
            // 遇到换行符时，oldPosition还停留在换行符，计算col时，会将换行符也算上，所以此处往后移动一个字符
            oldPosition++;
            line++;
            col = 0;
        } else {
            System.err.println(String.format("lex error at line %d:%d, unexpected char: %c(%d)", line, col, charAt(oldPosition), (int) charAt(oldPosition)));
            System.exit(-1);
        }
        col += srcPosition - oldPosition + 1;
        readTokens.add(token);
        return token;
    }

    /**
     * 转化十进制，包括小数
     */
    private Token parseDecimal() {
        Token token = new Token();
        token.type = Token.Type.NUMBER_VAL;
        int sum = charAt(srcPosition - 1) - '0';
        char c = charAt(srcPosition);
        while (c >= '0' && c <= '9') {
            sum = sum * 10 + c - '0';
            c = charAt(++srcPosition);
        }
        token.value = sum;
        // 遇到小数点，解析小数
        if (charAt(srcPosition) == '.') {
            double sum1 = 0;
            int radix = 10;
            token.type = Token.Type.FLOAT_VAL;
            c = charAt(++srcPosition);
            // 检查小数点后是不是数字
            if (!Character.isDigit(c)) {
                error("decimal value must be followed by number!");
            }
            while (Character.isDigit(c)) {
                sum1 += (c - '0') / 1.0 / radix;
                radix *= 10;
                c = charAt(++srcPosition);
            }
            token.value = sum / 1.0 + sum1;
        }
        return token;
    }

    private int parseHex() {
        char c = charAt(++srcPosition);
        int sum = 0;
        while (c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f') {
            int r = c - '0';
            if (c >= 'A')
                r = c - 'A';
            if (c >= 'a')
                r = c - 'a';
            sum = sum * 16 + r;
            c = charAt(++srcPosition);
        }
        return sum;
    }

    private int parseOct() {
        char c = charAt(srcPosition);
        int sum = c - '0';
        while (c >= '0' && c <= '7') {
            sum = sum * 8 + c - '0';
            c = charAt(++srcPosition);
        }
        return sum;
    }

    private void skipComment() {
        while (srcPosition < src.length() && charAt(srcPosition) != '\n')
            srcPosition++;
    }

    /**
     * 判断一个标识符，是标识符还是关键字
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

    public void unexpectedToken(Token.Type unexpectedType) {
        error("unexpected token type: " + unexpectedType.name());
    }

    public void unexpectedToken(Token.Type unexpectedType, Token.Type expectedType) {
        error("unexpected token type: " + unexpectedType.name() + ", expected is: " + expectedType.name());
    }

    public void error(String str) {
        throw new RuntimeException(String.format("at line %d:%d, %s", line, col, str));
    }

}