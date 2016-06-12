package banjo.expr.token;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.math.BigInteger;

import banjo.expr.util.FilePos;
import banjo.expr.util.FileRange;
import banjo.expr.util.ParserReader;
import banjo.expr.util.ParserReader.Pos;
import fj.data.List;

public class TokenScanner {
    boolean eof = false;

    public <T extends TokenVisitor<T>> T scan(String inStr, TokenVisitor<T> visitor) {
        try {
            return scan(ParserReader.fromString("<string>", inStr), visitor);
        } catch(final IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    public static boolean isIdentifierStart(int cp) {
        return cp == '_' || cp == '$' || cp == '\\' || cp == '∞' || Character.isLetter(cp);
    }

    public <T extends TokenVisitor<T>> T next(ParserReader in, TokenVisitor<T> visitor) throws IOException {
        for(;;) {
            in.getCurrentPosition(this.tokenStartPos);
            List<T> result = List.<T> nil();
            if((result = whitespace(in, visitor)).isNotEmpty() ||
                (result = comment(in, visitor)).isNotEmpty() ||
                (result = identifier(in, visitor)).isNotEmpty() ||
                (result = operator(in, visitor)).isNotEmpty() ||
                (result = numberLiteral(in, visitor)).isNotEmpty() ||
                (result = stringLiteral(in, visitor)).isNotEmpty()) {
                return result.head();
            } else {
                // Invalid character ?
                final int badCp = in.read();
                if(badCp == -1) {
                    this.eof = true;
                    return visitor.eof(in.getFileRange(FilePos.START));
                } else {
                    return visitor.badToken(in.getFileRange(this.tokenStartPos), codePointToString(badCp), "Unsupported code point " + badCp);
                }
            }
        }
    }

    /**
     * <p>
     * Scan the entire input up until EOF, passing tokens to the given visitor.
     *
     * <p>
     * Resets the scanner before scanning any tokens.
     *
     * @param in
     *            Input to parse
     * @param visitor
     *            Visitor to send events to
     * @return The return value from visitEof()
     * @throws IOException
     *             If the underlying stream cannot be read
     */
    public <T extends TokenVisitor<T>> T scan(ParserReader in, TokenVisitor<T> visitor) throws IOException {
        reset();
        for(;;) {
            final T result = next(in, visitor);
            if(this.eof)
                return result;
            visitor = result;
        }
    }

    public void reset() {
        this.eof = false;
    }

    static final int NBSP = '\u00A0';
    static final int NNBSP = '\u202F';
    static final int ELLIPSIS = '\u2026';


    public static boolean isIdentifierPart(int cp) {
        return isIdentifierStart(cp) || Character.isDigit(cp) || cp == NBSP || cp == NNBSP || cp == '\'';
    }

    StringBuffer buf = new StringBuffer(100);

    /**
     * Match an identifier at the current read position. Returns the identifier
     * string if there is a match, null otherwise. On failure the input position
     * will reset to the same position as before the call.
     *
     * @return A string if successful; null otherwise
     * @throws IOException
     */
    public String matchID(ParserReader in) throws IOException {
        final int first = in.read();
        if(!isIdentifierStart(first)) {
            in.unread();
            return null;
        }
        boolean escape = first == '\\';
        this.buf.setLength(0); // Reset buffer
        if(!escape)
            this.buf.appendCodePoint(first);
        for(;;) {
            final int cp = in.read();
            if(!escape && cp == '\\') {
                escape = true;
            } else if(!escape && cp == ' ') {
                in.getPreviousPosition(afterDigits);
                final int cp2 = in.read();
                if(!isIdentifierStart(cp2)) {
                    in.seek(afterDigits);
                    return this.buf.toString();
                }
                this.buf.appendCodePoint(cp);
                this.buf.appendCodePoint(cp2);
            } else {
                if(cp == -1 || !(escape || isIdentifierPart(cp))) {
                    in.unread();
                    return this.buf.toString();
                }
                this.buf.appendCodePoint(cp);
                escape = false;
            }
        }
    }

    public static final boolean isOperatorChar(int codePoint) {
        switch(codePoint) {
        case ',':
        case '"':
        case '\'':
        case '_':
        case '\\':
        case '∞':
            return false;
        case '-':
        case '^':
            return true;
        default:
        }
        switch(Character.getType(codePoint)) {
        case Character.MATH_SYMBOL:
        case Character.OTHER_SYMBOL:
        case Character.CONNECTOR_PUNCTUATION:
        case Character.OTHER_PUNCTUATION:
            return true;
        default:
            return false;
        }
    }

    public String matchOperator(ParserReader in) throws IOException {
        final int first = in.read();
        switch(first) {
        case ',':
            return ",";
        case ';':
            return ";";
        case '(':
            return "(";
        case ')':
            return ")";
        case '[':
            return "[";
        case ']':
            return "]";
        case '{':
            return "{";
        case '}':
            return "}";
        }

        if(!isOperatorChar(first)) {
            in.unread();
            return null;
        }

        in.getPreviousPosition(this.tokenStartPos);

        final int second = in.read();
        if(first == '.' && Character.isDigit(second)) {
            in.seek(this.tokenStartPos);
            return null;
        }

        // Operator '.' is special
        if(first == '.') {
            in.unread(); // unread the second character
            return ".";
        }
        if(!isOperatorChar(second)) {
            in.unread(); // unread the second character
            return String.copyValueOf(Character.toChars(first));
        }

        this.buf.setLength(0);
        this.buf.appendCodePoint(first);
        this.buf.appendCodePoint(second);
        for(;;) {
            final int cp = in.read();
            if(!isOperatorChar(cp)) {
                in.unread();
                break;
            }
            this.buf.appendCodePoint(cp);
        }
        return this.buf.toString();
    }

    public static boolean isWhitespaceChar(int codePoint) {
        return Character.isWhitespace(codePoint);
    }

    public <T extends TokenVisitor<T>> List<T> comment(ParserReader in, TokenVisitor<T> visitor) throws IOException {
        int cp = in.read();
        if(cp == '#') {
            skipToEndOfLine(in);
        } else {
            // Not a comment, and thus also not whitespace any more
            in.seek(this.tokenStartPos);
            return List.nil();
        }

        return List.<T> single(visitor.comment(in.getFileRange(this.tokenStartPos), in.readStringFrom(this.tokenStartPos)));
    }

    public <T extends TokenVisitor<T>> List<T> whitespace(ParserReader in, TokenVisitor<T> visitor) throws IOException {
        boolean foundWhitespace = false;
        for(int cp = in.read(); isWhitespaceChar(cp); cp = in.read()) {
            foundWhitespace = true;
        }
        in.unread(); // Push back the non-whitespace character we found
        if(!foundWhitespace)
            return List.nil(); // No whitespace found
        final FileRange fileRange = in.getFileRange(this.tokenStartPos);
        return List.<T> single(visitor.whitespace(fileRange, in.readStringFrom(this.tokenStartPos)));
    }

    /**
     * Return the number of characters of an identifier starting at the given
     * offset in the given string.
     *
     * @param in
     *            String to look at
     * @param start
     *            Starting offset to scan
     * @return The number of contiguous whitespace characters found
     */
    public static int scanIdentifierStart(String in, int start, int end) {
        if(start >= end)
            return 0;
        final int cp = in.codePointAt(start);
        final int n = ((start + 1) < end && cp == '\\') ? 2 : isIdentifierStart(cp) ? 1 : 0;
        if(n > 0)
            return n + scanIdentifierPart(in, start + n, end);
        return 0;
    }

    /**
     * Return the number of characters of an identifier continuing at the given
     * offset in the given string.
     *
     * @param in
     *            String to look at
     * @param start
     *            Starting offset to scan
     * @return The number of contiguous whitespace characters found
     */
    public static int scanIdentifierPart(String in, int start, int end) {
        for(int endOffset = start; endOffset < end;) {
            final int cp = in.codePointAt(endOffset);
            // TODO Handle spaces-in-names feature
            if(!isIdentifierPart(cp))
                return endOffset - start;
            endOffset += Character.charCount(cp);
        }
        return 0;
    }

    /**
     * Return the number of characters of whitespace at the given offset in the
     * given string.
     *
     * @param in
     *            String to look at
     * @param start
     *            Starting offset to scan
     * @return The number of contiguous whitespace characters found
     */
    public static int scanWhitespace(String in, int start, int end) {
        int endOffset = start;
        while(endOffset < end) {
            final int cp = in.codePointAt(endOffset);

            if(isWhitespaceChar(cp))
                endOffset += Character.charCount(cp);
            else
                break;
        }
        return endOffset - start;
    }

    /**
     * Return the length of any comment at the given offset in the given string.
     *
     * @param in
     *            String to look at
     * @param start
     *            Starting offset to scan
     * @return The length of the comment or zero if no comment found
     */
    public static int scanComment(String in, int start, int end) {
        for(int endOffset = start; endOffset < end;) {
            final int cp1 = in.codePointAt(endOffset);
            if(cp1 == '#' && endOffset + 1 < end) {
                final int cp2 = in.codePointAt(endOffset + 1);
                if(cp2 == '#') {
                    endOffset += 2 + scanToEndOfLine(in, endOffset + 2, end);
                    return endOffset - start;
                } else if(cp2 == '*') {
                    endOffset += 2 + scanToEndOfMultilineComment(in, endOffset + 2, end);
                    return endOffset - start;
                }
            }
            endOffset += Character.charCount(cp1);
        }
        // If we got here, it's not a comment
        return 0;
    }

    /**
     * Return the number of characters of whitespace and/or comments at the
     * given offset in the given string.
     *
     * @param in
     *            String to look at
     * @param start
     *            Starting offset to scan
     * @return The number of contiguous whitespace characters found
     */
    public static int scanWhitespaceAndComments(String in, int start, int end) {
        int endOffset = start;
        while(endOffset < end) {
            final int ws = scanWhitespace(in, endOffset, end);
            final int com = scanComment(in, endOffset + ws, end);
            endOffset += ws + com;
            if(ws == 0 && com == 0)
                break;
        }
        return endOffset - start;
    }

    private static int scanToEndOfMultilineComment(String in, int start, int end) {
        for(int endOffset = start; endOffset < end;) {
            final int cp1 = in.codePointAt(endOffset);
            if(cp1 == '*' && endOffset + 1 < end) {
                final int cp2 = in.codePointAt(endOffset + 1);
                if(cp2 == '/') {
                    return (endOffset + 2) - start;
                }
            }
            endOffset += Character.charCount(cp1);
        }
        return end - start;
    }

    private static int scanToEndOfLine(String in, int start, int end) {
        for(int endOffset = start; endOffset < end; endOffset++) {
            if(in.charAt(endOffset) == '\n')
                return endOffset + 1 - start;
        }
        return end - start;
    }

    /**
     * Assuming we just passed a '/' and a '*', read until we find the matching
     * '*' and '/' sequence.
     */
    public void skipToEndOfMultilineComment(ParserReader in) throws IOException {
        for(;;) {
            int cp = in.read();
            if(cp == '*' || cp == -1) {
                cp = in.read();
                if(cp == -1 || cp == '/')
                    break;
            }
        }
    }

    /**
     * Skip characters until we reach a new line
     */
    public void skipToEndOfLine(ParserReader in) throws IOException {
        final int currentLine = in.getCurrentLineNumber();
        while(in.read() != -1 && in.getCurrentLineNumber() == currentLine) {
            // continue
        }
    }

    private final Pos tokenStartPos = new Pos();

    /**
     * Check whether the input stream is positioned before a string literal and,
     * if so, parse it and return a StringLiteral node. If not, leave the input
     * in the same position as given and return null.
     *
     * The parser supports the following escape sequences (prefixed with a
     * backslash ('\') character:
     *
     * <table>
     * <thead>
     * <tr>
     * <td>Escape</td>
     * <td>Unicode Ordinal</td>
     * <td>Comment</td></thead> <tbody>
     * <tr>
     * <td>\</td>
     * <td>92</td>
     * <td>Literal Backslash</td>
     * </tr>
     * <tr>
     * <td>a</td>
     * <td>7</td>
     * <td>Bell</td>
     * </tr>
     * <tr>
     * <td>e</td>
     * <td>27</td>
     * <td>Escape</td>
     * </tr>
     * <tr>
     * <td>f</td>
     * <td>12</td>
     * <td>Form Feed</td>
     * </tr>
     * <tr>
     * <td>n</td>
     * <td>13</td>
     * <td>Carriage Return</td>
     * </tr>
     * <tr>
     * <td>r</td>
     * <td>10</td>
     * <td>Line Feed</td>
     * </tr>
     * <tr>
     * <td>t</td>
     * <td>9</td>
     * <td>Horizontal Tab</td>
     * </tr>
     * <tr>
     * <td>ooo</td>
     * <td></td>
     * <td>One to three octal digits will be interpreted as an octal number in
     * the range 0 to 0777 (decimal 1024)</td>
     * </tr>
     * <tr>
     * <td>xXX{2}</td>
     * <td></td>
     * <td>Two hex digits give a ASCII character code in the range 0-127</td>
     * </tr>
     * <tr>
     * <td>uXXXX</td>
     * <td></td>
     * <td>Four hex digits give a unicode character in the range 0-65535</td>
     * </tr>
     * </tbody>
     * </table>
     */

    private <T extends TokenVisitor<T>> List<T> stringLiteral(ParserReader in, TokenVisitor<T> visitor) throws IOException {
        int openQuoteIndentColumn = in.getCurrentIndentColumn();
        int cp = in.read();
        if(cp == '`') {
            return backtick(in, visitor);
        }
        if(cp != '"' && cp != '\'') {
            in.seek(this.tokenStartPos);
            return List.nil();
        }
        final int quoteType = cp;

        // Minimum indent column
        int indentColumn = openQuoteIndentColumn;
        boolean multiline = false;
        this.buf.setLength(0);
        List<T> errs = List.nil();
        while((cp = in.read()) != -1) {
            final int currentColumnNumber = in.getPreviousColumnNumber();
            // Check for the end of the string
            if(cp == quoteType && !(multiline && currentColumnNumber != openQuoteIndentColumn))
                break;

            // Completely ignore/strip literal line feeds
            if(cp == '\r')
                continue;

            // If we get a newline at the start of the string,
            // it's a multiline string. Otherwise, it's an error
            // (missing close quote).
            if(cp == '\n') {
                if(buf.length() == 0) {
                    multiline = true;
                    indentColumn = openQuoteIndentColumn;
                    continue;
                } else if(!multiline) {
                    return errs.snoc(
                        visitor.badToken(
                            in.getFileRange(this.tokenStartPos),
                            "",
                            "Missing close quote " + StringLiteral.toSource(String.valueOf(Character.toChars(quoteType)))));
                }
            } else if(multiline) {
                if(cp == ' ' && buf.length() == 0) {
                    indentColumn = Math.max(currentColumnNumber, indentColumn);
                    continue;
                }

                // Each first line should be indented beyond the one
                // with the open quote
                if(currentColumnNumber <= indentColumn) {
                    // If we got whitespace, throw it away
                    if(cp == ' ') {
                        continue;
                    }

                    // Anything else is an error
                    in.unread();
                    return errs.snoc(
                        visitor.badToken(
                            in.getFileRange(this.tokenStartPos),
                            "",
                            "Dedent in string literal, or missing close quote " + StringLiteral.toSource(String.valueOf(Character.toChars(quoteType)))));
                }
            }

            if(cp != '\\') {
                this.buf.appendCodePoint(cp);
                continue;
            }
            final FilePos afterBackslash = in.getFilePos();
            cp = in.read();
            if(cp == -1) {
                return errs.snoc(visitor.badToken(in.getFileRange(this.tokenStartPos), "", "Backslash at end of file."));
            }
            switch(cp) {
            case '\\':
                this.buf.append('\\');
                break;
            case 'a':
                this.buf.append('\007');
                break;
            case 'e':
                this.buf.append('\033');
                break;
            case 'r':
                this.buf.append('\r');
                break;
            case 'n':
                this.buf.append('\n');
                break;
            case 'f':
                this.buf.append('\f');
                break;
            case 't':
                this.buf.append('\t');
                break;
            case '\'':
            case '"':
                this.buf.append((char) cp);
                break;
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7': {
                octalEscape(in, cp, this.buf);
                break;
            }

            case 'x': {
                errs = errs.append(hexEscape(in, this.buf, 2, visitor));
                break;
            }

            case 'u': {
                errs = errs.append(hexEscape(in, this.buf, 4, visitor));
                break;
            }

            default: {
                final String escSeq = StringLiteral.toSource(in.readStringFrom(afterBackslash));
                return List.<T> single(visitor.badToken(in.getFileRange(afterBackslash), escSeq, "Unknown escape sequence " + escSeq));
            }
            }
        }
        if(cp == -1)
            errs = errs.snoc(visitor.badToken(in.getFileRange(this.tokenStartPos), "", "End of file in string literal"));
        if(errs.isNotEmpty())
            return errs;
        return List.<T> single(visitor.stringLiteral(in.getFileRange(this.tokenStartPos), openQuoteIndentColumn, this.buf.toString()));
    }

    private <T extends TokenVisitor<T>> List<T> backtick(ParserReader in, TokenVisitor<T> visitor) throws IOException {
        int indentColumn = in.getCurrentIndentColumn();
        String str = matchOperator(in);
        if(str == null)
            str = matchID(in);
        if(str == null) {
            visitor.badToken(in.getFileRange(this.tokenStartPos), in.readStringFrom(this.tokenStartPos), "Empty backtick");
            str = "";
        }
        return List.<T> single(visitor.stringLiteral(in.getFileRange(this.tokenStartPos), indentColumn, str));
    }

    private final Pos afterDigits = new Pos();

    private <T extends TokenVisitor<T>> List<T> hexEscape(ParserReader in, StringBuffer buf, final int digitCount, TokenVisitor<T> visitor) throws IOException {
        int result = 0;
        in.getCurrentPosition(this.afterDigits);
        for(int digits = 0; digits < digitCount; digits++) {
            final int digitValue = Character.digit(in.read(), 16);
            if(digitValue == -1) {
                T err = visitor.badToken(in.getFileRange(this.afterDigits), in.readStringFrom(this.afterDigits), "Invalid hex digit in \\x escape");
                in.seek(this.afterDigits);
                return List.<T> single(err);
            } else {
                result = (result << 4) | digitValue;
                in.getCurrentPosition(this.afterDigits);
            }
        }
        buf.appendCodePoint(result);
        return List.nil();
    }

    private void octalEscape(ParserReader in, int cp, StringBuffer buf) throws IOException {
        int result = Character.digit(cp, 8);
        in.getCurrentPosition(this.afterDigits);
        for(int digits = 1; digits < 3; digits++) {
            final int digitValue = Character.digit(in.read(), 8);
            if(digitValue == -1) {
                in.seek(this.afterDigits);
                return;
            } else {
                result = (result << 3) + digitValue;
                in.getCurrentPosition(this.afterDigits);
            }
        }
        buf.appendCodePoint(result);
    }

    private <T extends TokenVisitor<T>> List<T> numberLiteral(ParserReader in, TokenVisitor<T> visitor) throws IOException {
        int indentColumn = in.getCurrentIndentColumn();
        int cp = in.read();
        boolean negative = false;
        boolean isNumber = false;
        boolean isInteger = true;
        if(cp == '-') {
            // Leading '-' for a negative number of any kind
            negative = true;
            cp = in.read();
        } else if(cp == '+') {
            // Leading + is OK (but useless)
            cp = in.read();
        }
        // Check for a different base
        int radix = 10;
        String formatName = "decimal";
        int maxLongDigits = 18; // the most decimal digits that can SAFELY be
                                // kept in a 62-bit integer (we leave one for
                                // the sign bit)
        int digits = 0;
        if(cp == '0') {
            isNumber = true;
            cp = in.read();
            switch(cp) {
            case 'x':
            case 'X':
                radix = 16;
                maxLongDigits = 15;
                formatName = "hexadecimal";
                cp = in.read();
                break;
            case 'b':
            case 'B':
                radix = 2;
                maxLongDigits = 62;
                formatName = "binary";
                cp = in.read();
                break;
            case 'o':
            case 'O':
                radix = 8;
                maxLongDigits = 20;
                formatName = "octal";
                cp = in.read();
                break;
            default:
                digits = 1;
                in.unread();
                cp = '0';
                in.getCurrentPosition(this.afterDigits);
                break;
            }
        }
        int digitsLeftOfDecimalPoint = digits;
        int exp = 0;
        long intValLong = 0;
        BigInteger intValBig = null;
        BigInteger radixBig = null;
        int expType = -1;
        for(;;) {
            if(cp == '.') {
                if(radix != 10) {
                    visitor
                        .badToken(in.getFileRange(this.afterDigits), in.readStringFrom(this.afterDigits), "Decimal point found in " + formatName + " number");
                    radix = 10;
                }
                if(!isInteger) {
                    if(!isNumber) {
                        in.seek(this.tokenStartPos);
                        return List.nil();
                    } else {
                        break;
                    }
                } else {
                    digitsLeftOfDecimalPoint = digits;
                    isInteger = false;
                    cp = in.read();
                }
            } else if(digits > 0 && cp == '_') {
                // Allow underscore to "break up" long numbers, like in Java
                cp = in.read();
            } else if(radix == 10 && (cp == 'e' || cp == 'E' || cp == 'f' || cp == 'd' || cp == 'D')) {
                // Can't start a number with an exponent marker
                if(!isNumber) {
                    in.seek(this.tokenStartPos);
                    return List.nil();
                }
                expType = cp;

                in.getCurrentPosition(this.afterDigits);
                cp = in.read();
                final boolean negexp = cp == '-';
                if(negexp || cp == '+')
                    cp = in.read();
                for(int expDigits = 0; expDigits < 10; expDigits++) {
                    if(cp == '_' && expDigits > 0) {
                        cp = in.read();
                    }
                    final int digitValue = Character.digit(cp, 10); // Always
                                                                    // base 10
                                                                    // because
                                                                    // d, e, and
                                                                    // f are hex
                                                                    // digits
                    if(digitValue == -1) {
                        break;
                    }
                    exp = (exp * 10) + digitValue;
                    in.getCurrentPosition(this.afterDigits);
                    cp = in.read();
                }
                if(negexp)
                    exp = -exp;
                break;
            } else if(cp == 'L' || cp == 'i') {
                expType = cp;
                break; // No more digits after the suffix
            } else if(cp == 'I') {
                // Force BigInteger
                expType = cp;
                if(intValBig == null)
                    intValBig = BigInteger.valueOf(intValLong);
                break; // No more digits after the 'I'
            } else {
                final int digitValue = Character.digit(cp, radix);
                if(digitValue == -1) {
                    if(!isNumber) {
                        in.seek(this.tokenStartPos);
                        return List.nil();
                    } else {
                        // Number ends when we find any non-number character
                        break;
                    }
                } else {
                    if(intValBig != null) {
                        intValBig = intValBig.multiply(radixBig).add(BigInteger.valueOf(digitValue));
                    } else if(digits == maxLongDigits) {
                        radixBig = BigInteger.valueOf(radix);
                        intValBig = BigInteger.valueOf(intValLong).multiply(radixBig).add(BigInteger.valueOf(digitValue));
                    } else {
                        intValLong = (intValLong * radix) + digitValue;
                    }
                    digits++;
                    isNumber = true;
                    in.getCurrentPosition(this.afterDigits);
                    cp = in.read();
                }
            }
        }

        // Back out the last character we read, it's not part of the number
        in.seek(this.afterDigits);

        final int digitsRightOfDecimalPoint = digits - digitsLeftOfDecimalPoint;

        // Check for a period without digits after it, in which case this
        // might be an integer projection or not a number (e.g. an operator).
        if(!isInteger && digits == digitsLeftOfDecimalPoint) {
            if(!isNumber) {
                in.seek(this.tokenStartPos);
                return List.nil();
            } else {
                in.seek(this.afterDigits);
                isInteger = true;
            }
        }

        final String text = in.readStringFrom(this.tokenStartPos);
        final int scale = digitsRightOfDecimalPoint - exp; // TODO Check for
                                                           // overflow on the
                                                           // scale; we might
                                                           // have to restrict
                                                           // scale to 32 bits
        Number number;
        try {
            if(negative) {
                intValLong = -intValLong;
                if(intValBig != null)
                    intValBig = intValBig.negate();
            }
            if(intValBig == null) {
                if(isInteger) {
                    number = BigInteger.valueOf(intValLong);
                } else {
                    number = BigDecimal.valueOf(intValLong, scale);
                }
            } else {
                if(isInteger) {
                    number = intValBig;
                } else {
                    number = new BigDecimal(intValBig, scale);
                }
            }
            switch(expType) {
            case 'f':
                final float floatValue = number.floatValue();
                if(!Float.isFinite(floatValue))
                    throw new ArithmeticException("Number too large for float: " + text);
                number = floatValue;
                break;
            case 'd':
                final double doubleValue = number.doubleValue();
                if(!Double.isFinite(doubleValue))
                    throw new ArithmeticException("Number too large for double: " + text);
                number = doubleValue;
                break;
            case 'D':
                if(isInteger) {
                    if(intValBig == null) {
                        number = BigDecimal.valueOf(intValLong, scale);
                    } else {
                        number = new BigDecimal(intValBig, scale);
                    }
                }
                break;
            case 'i':
                // TODO Check if out of range
                number = number.intValue();
                break;
            case 'L':
                // TODO Check if out of range
                number = number.longValue();
                break;
            }
        } catch(ArithmeticException e) {
            visitor.badToken(in.getFileRange(this.tokenStartPos), in.readStringFrom(this.tokenStartPos), e.getMessage());
            number = Long.valueOf(0);
        }
        final FileRange fileRange = in.getFileRange(this.tokenStartPos);
        return List.single(visitor.numberLiteral(fileRange, indentColumn, number, text));
    }

    /**
     * Parse a simple identifier expression. If the current parse position has
     * an identifier, this consumes it and returns it. Otherwise, this resets
     * the parse position and returns null.
     *
     * @return An IdRef if the parse is successful; null otherwise.
     */
    private <T extends TokenVisitor<T>> List<T> identifier(ParserReader in, TokenVisitor<T> visitor) throws IOException {
        int indentColumn = in.getCurrentIndentColumn();
        final String identifier = matchID(in);
        if(identifier == null)
            return List.nil();
        return List.single(visitor.identifier(in.getFileRange(this.tokenStartPos), indentColumn, identifier));
    }

    /**
     * Parse an operator. If the current parse position has an operator, this
     * consumes it and returns it. Otherwise, this resets the parse position and
     * returns null.
     *
     * @return An OperatorRef if the parse is successful; O otherwise
     */
    private <T extends TokenVisitor<T>> List<T> operator(ParserReader in, TokenVisitor<T> visitor) throws IOException {
        int indentColumn = in.getCurrentIndentColumn();
        final String operator = matchOperator(in);
        if(operator == null)
            return List.nil();
        return List.single(visitor.operator(in.getFileRange(this.tokenStartPos), indentColumn, operator));
    }

    public static String codePointToString(int cp) {
        return new String(Character.toChars(cp));
    }
}
