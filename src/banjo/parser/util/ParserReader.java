package banjo.parser.util;

import java.io.BufferedReader;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.net.URL;
import java.net.URLConnection;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;



/**
 * A reader that keeps track of the file line and column information.
 *
 */
public class ParserReader extends Reader {

	public static class Pos {
		int line=1;
		int col=1;
		int offset=0;
		int indentLevel=0;
		boolean startOfLine;

		public Pos() {
		}
		public Pos(Pos other) {
			assign(other);
		}
		public void assign(Pos other) {
			this.line = other.line;
			this.col = other.col;
			this.offset = other.offset;
			this.indentLevel = other.indentLevel;
		}
		public void assign(FilePos filePos) {
			this.line = filePos.line;
			this.col = filePos.column;
			this.offset = filePos.offset;
		}


		private void accumulate(int ch) {
			if(ch == -1) {
				if(this.col != 1) {
					this.line++;
					this.col = 1;
				}
				return;
			}

			if(ch == '\n') {
				this.line++;
				this.col = 1;
				this.startOfLine=true;
			} else {
				this.col++;
				if(ch == ' ' && this.startOfLine) this.indentLevel++;
				else this.startOfLine = false;
			}
			this.offset++;
		}
		public FilePos toFilePos() {
			return new FilePos(this.offset, this.line, this.col);
		}
		@Override
		public Pos clone() {
			return new Pos(this);
		}

		@Override
		public String toString() {
			return "line "+this.line+" col "+this.col+ " offset "+this.offset;
		}
		public int getLine() {
			return this.line;
		}
		public int getColumn() {
			return this.col;
		}
		public int getOffset() {
			return this.offset;
		}
		public int getIndentLevel() {
			return this.indentLevel;
		}
		public boolean isStartOfLine() {
			return this.startOfLine;
		}
		public void moveToStartOfLine() {
			this.offset -= this.col-1;
			this.col = 1;
		}

	}

	final Reader delegate;

	final Pos current = new Pos();
	final Pos previous = new Pos(); // Previous position
	final Pos mark = new Pos();
	public final int fileSize; // In chars

	// TODO Support different kinds of line delimiters
	final char[][] lineDelimiters;
	public static final char[][] DEFAULT_LINE_DELIMITERS = {{'\n'}};

	@Override
	public int read(CharBuffer target) throws IOException {
		if(target == null) throw new NullPointerException();
		final int offset = target.position();
		final int charsRead = this.delegate.read(target);
		for(int i=0; i < charsRead; i++) {
			accumulate(target.get(offset+i));
		}
		return charsRead;
	}

	@Override
	public int read() throws IOException {
		final int ch = this.current.offset >= this.fileSize ? -1 : this.delegate.read();
		accumulate(ch);
		return ch;
	}

	private void accumulate(int ch) {
		this.previous.assign(this.current);
		this.current.accumulate(ch);
	}

	@Override
	public int read(char [] cbuf) throws IOException {
		if(cbuf == null) throw new NullPointerException();
		final int len = this.delegate.read(cbuf);
		for(int i=0; i < len; i++) {
			accumulate(cbuf[i]);
		}
		return len;
	}

	@Override
	public int read(char [] cbuf, int off, int len) throws IOException {
		if(cbuf == null) throw new NullPointerException();
		final int lenRead = this.delegate.read(cbuf, off, len);
		for(int i=0; i < lenRead; i++) {
			accumulate(cbuf[off+i]);
		}
		return lenRead;
	}

	@Override
	public long skip(long n) throws IOException {
		// We need to keep our line number accurate
		for(long skipped=0; skipped < n; skipped++) {
			if(read() == -1)
				return skipped;
		}
		return n;
	}

	@Override
	public boolean ready() throws IOException {
		return this.delegate.ready();
	}

	@Override
	public boolean markSupported() {
		return true;
	}

	@Override
	public void mark(int readAheadLimit) throws IOException {
		this.delegate.mark(readAheadLimit);
		this.mark.assign(this.current);
	}

	/**
	 * Reset to the last mark.
	 */
	@Override
	public void reset() throws IOException {
		this.delegate.reset();
		this.previous.assign(this.current);
		this.current.assign(this.mark);
	}

	/**
	 * Reset to a given absolute offset in characters.
	 * <p>
	 * Note that you cannot seek to before the mark, so call mark() only if
	 * you know you will not seek before that mark.
	 * <p>
	 * However, this will be faster if you do call mark() periodically,
	 * since it will seek back to the last mark and then scan characters
	 * to recalculate the line number at the target position.
	 *
	 * @throws IndexOutOfBoundsException If the offset provided is before the last mark or beyond the end of the file
	 */
	public void seek(int offset) throws IOException {
		if(offset > this.fileSize)
			throw new IndexOutOfBoundsException("Past EOF");

		if(offset == this.current.offset) {
			// Do nothing, we're already there
		} else if(offset > this.current.offset) {
			// Scan ahead
			skip(offset-this.current.offset);
		} else if(offset == this.mark.offset) {
			// Jump back to the mark
			reset();
		} else if(offset == this.previous.offset) {
			seek(this.previous);
		} else if(offset > this.mark.offset) {
			// Save some line/column calculations if we're seeking back within the current line
			final int charsBack = this.current.offset - offset;
			final boolean sameLineAsCurrent = charsBack < this.current.col;
			if(sameLineAsCurrent) {
				// Reposition the reader on the desired character
				this.delegate.reset();
				this.delegate.skip(offset-this.mark.offset);

				// Just directly calculate the column number
				this.current.col -= charsBack;
				this.current.offset = offset;
			} else {
				// Jump back to the mark, then scan ahead to the given offset while recomputing the file offset
				// This is slower than the above
				reset();
				skip(offset - this.mark.offset);
			}
		} else {
			throw new IndexOutOfBoundsException("Cannot scan back past the last mark.");
		}
	}

	/**
	 * Read the file position from the parameter and seek to that position, if possible.
	 * <p>
	 * Note that this will assume that the line and column information provided are
	 * correct for the given offset.
	 *
	 * @see #seek(int)
	 */
	public void seek(Pos offset) throws IOException {
		if(offset.offset == this.current.offset) {
			// Do nothing, we're already there
		} else if(offset.offset == this.mark.offset) {
			// Jump back to the mark
			reset();
		} else if(offset.offset > this.mark.offset){
			this.delegate.reset();
			this.delegate.skip(offset.offset-this.mark.offset);
			this.current.assign(offset);
		} else {
			throw new IndexOutOfBoundsException("Cannot scan back past the last mark.");
		}
	}

	/**
	 * Read the file position from the parameter and seek to that position, if possible.
	 * <p>
	 * Note that this will assume that the line and column information provided are
	 * correct for the given offset.  To seek to just an offset, call seek(int).
	 *
	 * @see #seek(int)
	 */
	public void seek(FilePos filePos) throws IOException {
		if(filePos.offset == this.current.offset) {
			// Do nothing, we're already there
		} else if(filePos.offset == this.mark.offset) {
			// Jump back to the mark
			reset();
		} else if(filePos.offset == this.previous.offset) {
			seek(this.previous);
		} else if(filePos.offset > this.current.offset) {
			skip(filePos.offset - this.current.offset);
		} else if(filePos.offset > this.mark.offset){
			this.delegate.reset();
			this.delegate.skip(filePos.offset-this.mark.offset);
			this.current.assign(filePos);
		} else {
			throw new IndexOutOfBoundsException("Cannot scan back past the last mark.");
		}
	}

	/**
	 * Seek to the end of the given range.  The range is assumed to have come from
	 * this file and to have a correct end line and column in it.
	 * <p>
	 * After this call, the file position will be set to read the character
	 * immediately following the provided range.
	 *
	 * @param range Range to use as a reference.
	 * @throws IOException If bytes had to be read from the the file and an error occurred while doing so
	 */
	public void seekPast(final FileRange fileRange) throws IOException {
		this.seek(fileRange.getEndOffset());
	}

	/**
	 * Get the current offset into the source, in characters.  This starts at 0.
	 */
	public int getCurrentOffset() {
		return this.current.offset;
	}

	/**
	 * Get the current line number in the source.  This starts at 1.
	 */
	public int getCurrentLineNumber() {
		return this.current.line;
	}

	/**
	 * Get the current column number in the source.  This starts at 1.
	 */
	public int getCurrentColumnNumber() {
		return this.current.col;
	}

	@Override
	public void close() throws IOException {
		this.delegate.close();
	}

	public int remaining() {
		return this.fileSize - this.current.offset;
	}

	/**
	 * Create a CharSequence wrapping this reader at its current position.
	 *
	 * Don't use the Reader and the CharSequence in parallel.  Note that
	 * the CharSequence reads ahead and may advance the reader position arbitrarily;
	 * use mark() and reset() to restore the file position after using
	 * this feature.
	 */
	public CharSequence toCharSequence() {
		return new ReaderCharSequence(this, remaining(), 1024);
	}

	/**
	 * Attempt to match the given regular expression against the next
	 * available characters in the stream.
	 *
	 * Returns a Matcher indicating the result of the match.
	 *
	 * This method will advance the file pointer arbitrarily; be sure to
	 * use reset() when you are done with the matcher to go to your
	 * desired file position.
	 */
	public Matcher matcher(Pattern p) throws IOException {
		final Matcher result = p.matcher(toCharSequence());
		if(result == null) throw new NullPointerException();
		return result;
	}

	/**
	 * Get the current file position as a FilePos instance.
	 */
	public FilePos getFilePos() {
		return this.current.toFilePos();
	}

	/**
	 * Create a new parser reader.
	 *
	 * @param delegate Reader to delegate to.  If it doesn't support marks, this will
	 *                 wrap it in a buffered reader capable of buffering the entire file.
	 *                 If it does support marks, it should never invalidate the mark
	 *                 no matter how many characters are read.  This calls mark() immediately
	 *                 to allow "seeking" within the stream.
	 * @param fileSize Total length of the file
	 * @throws IOException If there's a problem getting the length of the file
	 */
	public ParserReader(Reader delegate, int fileSize) throws IOException {
		if(fileSize < 0) throw new IllegalArgumentException("Negative fileSize");
		if(!delegate.markSupported()) {
			delegate = new BufferedReader(delegate, fileSize);
		}
		this.delegate = delegate;
		this.fileSize = fileSize;
		this.lineDelimiters = DEFAULT_LINE_DELIMITERS;
		mark();
	}

	/**
	 * Create a new parser reader from a URL.
	 *
	 * @param url URL to load
	 * @throws IOException If there's a problem connecting to the given URL or getting the length of the file
	 */
	public ParserReader(URL url) throws IOException {
		final URLConnection c = url.openConnection();
		final String urlString = url.toString();
		if(urlString == null) throw new NullPointerException();
		this.fileSize = c.getContentLength();
		if(this.fileSize == -1)
			throw new UnsupportedOperationException("Don't yet support reading a file where we don't know the file size in advance.");
		String contentEncoding = c.getContentEncoding();
		if(contentEncoding == null) contentEncoding = Charset.defaultCharset().name();
		this.delegate = new BufferedReader(new InputStreamReader(c.getInputStream(), contentEncoding), this.fileSize);
		this.lineDelimiters = DEFAULT_LINE_DELIMITERS;
		mark();
	}

	/**
	 * Create a ParserReader from a string.
	 *
	 * @param filename Filename to use for error reporting
	 * @param body Body of the string
	 * @return A new parser reader
	 */
	public static ParserReader fromString(String filename, String body) {
		try {
			return new ParserReader(new StringReader(body), body.length());
		} catch (final IOException e) {
			throw new Error(e);
		}
	}

	/**
	 * Parse a substring of the given string.
	 *
	 * <p> This may eventually be implemented so it doesn't copy parts the original string but rather
	 * pretends to have an EOF at the given offset.
	 *
	 * @param filename File to use for reporting errors
	 * @param body Text to use an input
	 * @param beginIndex Position to start at (it will scan to this position and calculate the line/column information)
	 * @param endIndex Position to stop at; it will report EOF at that character offset.
	 * @return
	 */
	public static ParserReader fromSubstring(String filename, String body, int beginIndex, int endIndex) {
		try {
			final ParserReader reader = new ParserReader(new StringReader(body), endIndex);
			reader.skip(beginIndex);
			return reader;
		} catch (final IOException e) {
			throw new Error(e);
		}
	}

	public void mark() throws IOException {
		mark(remaining());
	}

	/**
	 * Return the file range from the given position to the current position.
	 */
	public FileRange getFileRange(FilePos from) {
		return new FileRange(from, this.current.toFilePos());
	}

	/**
	 * Return the file range from the given position to the current position.
	 */
	public FileRange getFileRange(Pos from) {
		return new FileRange(from.toFilePos(), this.current.toFilePos());
	}

	/**
	 * Read any input matching the given terminals and return it as a string.
	 * <p>
	 * The input is positioned immediately after the last character matched.
	 * <p>
	 * Returns null if the regular expression doesn't match.
	 */
	public String consume(Pattern re) throws IOException {
		final Pos start = new Pos(this.current);
		final Matcher m = matcher(re);
		if(m.lookingAt() && m.end() > m.start()) {
			// Position just at the end of the token that was matched
			final String text = Objects.requireNonNull(m.group());
			seek(start);
			skip(m.end());
			return text;
		}
		seek(start);
		return "";
	}

	public String readString(int chars) throws IOException {
		final char[] buf = new char[chars];
		int remaining = chars;
		while(remaining > 0) {
			final int didRead = read(buf);
			if(didRead == -1) throw new EOFException();
			remaining -= didRead;
		}
		return new String(buf);
	}

	/**
	 * Check whether the given string matches the characters following
	 * the current read position.
	 * <p>
	 * If they do, the read position is advanced beyond the end of the characters
	 * that matched.  Otherwise, the file position will be reset back to the same
	 * position as before this call was made.
	 *
	 * @param expected String to check for
	 * @return True if the next characters match the string, or if the string was empty
	 */
	public boolean startsWith(String expected) throws IOException {
		if(expected.length() > remaining())
			return false;
		final FilePos start = getFilePos();
		final int len = expected.length();
		for(int i=0; i < len; i++) {
			final int ch = read();
			if(ch != expected.charAt(i)) {
				seek(start);
				return false;
			}
		}
		return true;
	}

	public boolean startsWith(char expected) throws IOException {
		if(remaining() == 0)
			return false;
		final FilePos start = getFilePos();
		if(read() != expected) {
			seek(start);
			return false;
		}
		return true;
	}

	/**
	 * @return a FileRange of length zero at the current file position.
	 */
	public FileRange getFilePosAsRange() {
		final FilePos pos = getFilePos();
		return new FileRange(pos, pos);
	}

	/**
	 * Seek to the beginning of the line containing the given file position,
	 * and then read to the end of that line.  The file position is left at
	 * the end of the line.
	 * <p>
	 * A line is considered to end at either a newline character ('\n') or
	 * the end of the file.
	 */
	public String readLineContaining(FilePos pos) throws IOException {
		seek(pos.lineStart());
		return readToEndOfLine();
	}

	/**
	 * Read from the current file position to the end of the line.
	 */
	public String readToEndOfLine() throws IOException {
		final StringBuffer sb = new StringBuffer();
		for(;;) {
			final int ch = read();
			if(ch == '\n' || ch == -1) {
				final String s = sb.toString();
				if(s == null) throw new NullPointerException();
				return s;
			}
			sb.append((char)ch);
		}
	}

	public String positionInLineAsString() throws IOException {
		final Pos start = new Pos(this.current);
		final int colToMark = this.current.col;
		start.moveToStartOfLine();
		seek(start);
		final StringBuffer sb = new StringBuffer();
		final String line = readToEndOfLine();
		sb.append(line).append("\n");
		for(int i=0; i <= line.length(); i++) {
			sb.append((i+1)==colToMark?'^':' ');
		}
		sb.append('\n');
		seek(start);
		return sb.toString();
	}

	/**
	 * Create a FileRange that is a subRange of the given token.  This ensures that
	 * line numbers are calculated appropriately.
	 *
	 * @param token Token to use for the range
	 * @param startOffset Offset from the token's start to start the range in
	 * @param endOffset Offset from the token's start to end the range in (exclusive)
	 * @return A new FileRange
	 * @throws IOException
	 */
	public FileRange subRange(FileRange range, int startOffset, int endOffset) throws IOException {
		if(endOffset < startOffset) throw new IllegalArgumentException("End offset must be greater than start offset");

		final FilePos curPos = getFilePos();
		try {
			seek(range.getStart());
			skip(startOffset);
			final FilePos rangeStart = getFilePos();
			skip(endOffset - startOffset);
			return getFileRange(rangeStart);
		} finally {
			seek(curPos);
		}
	}

	/**
	 * Read a string starting at the given file position up to the current position.  Useful if you have been parsing some stuff
	 * and want to bundle up everything you parsed into a new Token.
	 *
	 * @param start Start position of the token to read
	 * @return A new String taken from that range
	 * @throws IOException If there's a problem reading the data
	 */
	public String readStringFrom(Pos start) throws IOException {
		final int endOffset = this.current.offset;
		seek(start);
		return readString(endOffset - start.offset);
	}

	/**
	 * Read a string starting at the given file position up to the current position.  Useful if you have been parsing some stuff
	 * and want to bundle up everything you parsed into a new Token.
	 *
	 * @param start Start position of the token to read
	 * @return A new String taken from that range
	 * @throws IOException If there's a problem reading the data
	 */
	public String readStringFrom(FilePos start) throws IOException {
		final int endOffset = this.current.offset;
		seek(start);
		return readString(endOffset - start.offset);
	}


	/**
	 * @return true if we've seen nothing but space characters since the last newline.
	 */
	public boolean isAtStartOfLine() { return this.current.startOfLine; }

	/**
	 * Copy the current position into the given position object.
	 * <p>
	 * Returns its argument.
	 */
	public Pos getCurrentPosition(Pos into) {
		into.assign(this.current);
		return into;
	}

	/**
	 * Copy the position we were at prior to the last read().
	 */
	public Pos getPreviousPosition(Pos into) {
		into.assign(this.previous);
		return into;
	}

	/**
	 * Reset the read position to before the last code point read.
	 */
	public void unread() throws IOException {
		seek(this.previous);
	}

	/**
	 * Read, unread, and return the next character.  The file position is
	 * left unchanged although the unread() position is affected.
	 * @throws IOException
	 */
	public int peek() throws IOException {
		final int cp = read();
		unread();
		return cp;
	}

	/**
	 * If the next character matches the given code point, consume it
	 * and return true.  Otherwise, unread() it and return false.
	 */
	public boolean checkNextChar(int codePoint) throws IOException {
		if(read() == codePoint) {
			return true;
		} else {
			unread();
			return false;
		}
	}

	/**
	 * Return the number of characters since the given start position.
	 */
	public int getLength(Pos tokenStartPos) {
		return getCurrentOffset() - tokenStartPos.getOffset();
	}

	public FileRange calcRange(int sourceOffset, int sourceLength) throws IOException {
		seek(sourceOffset);
		final FilePos start = getFilePos();
		skip(sourceLength);
		return getFileRange(start);
	}

	public String readString(FileRange range) throws IOException {
		seek(range.getStart());
		return readString(range.length());
	}


}
