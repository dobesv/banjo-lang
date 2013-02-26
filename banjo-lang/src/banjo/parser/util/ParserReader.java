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
import java.util.regex.Matcher;
import java.util.regex.Pattern;



/**
 * A reader that keeps track of the file line and column information.
 * 
 */
public class ParserReader extends Reader {

	static class Pos {
		int line=1;
		int col=1;
		int offset=0;
		
		public Pos() {
		}
		public Pos(Pos other) {
			assign(other);
		}
		public void assign(Pos other) {
			this.line = other.line;
			this.col = other.col;
			this.offset = other.offset;
		}
		public void assign(FilePos filePos) {
			this.line = filePos.line;
			this.col = filePos.column;
			this.offset = filePos.offset;
		}
		
		
		private void accumulate(int ch) {
			if(ch == -1)
				return;
			
			if(ch == '\n') {
				line++;
				col = 1;
			} else {
				col++;
			}
			offset++;
		}
		public FilePos toFilePos() {
			return new FilePos(offset, line, col);
		}
		
		@Override
		public String toString() {
			return "line "+line+" col "+col+ " offset "+offset;
		}
		
	}
	
	final Reader delegate;
	final String filename;
	
	final Pos current = new Pos();
	final Pos mark = new Pos();
	public final int fileSize; // In chars
	
	public int read(CharBuffer target) throws IOException {
		int offset = target.position();
		int charsRead = delegate.read(target);
		for(int i=0; i < charsRead; i++) {
			accumulate(target.get(offset+i));
		}
		return charsRead;
	}

	public int read() throws IOException {
		int ch = delegate.read();
		accumulate(ch);
		return ch;
	}

	private void accumulate(int ch) {
		current.accumulate(ch);
	}

	public int read(char[] cbuf) throws IOException {
		int len = delegate.read(cbuf);
		for(int i=0; i < len; i++) {
			accumulate(cbuf[i]);
		}
		return len;
	}

	public int read(char[] cbuf, int off, int len) throws IOException {
		int lenRead = delegate.read(cbuf, off, len);
		for(int i=0; i < lenRead; i++) {
			accumulate(cbuf[off+i]);
		}
		return lenRead;
	}

	public long skip(long n) throws IOException {
		// We need to keep our line number accurate
		for(long skipped=0; skipped < n; skipped++) {
			if(read() == -1)
				return skipped;
		}
		return n;
	}

	public boolean ready() throws IOException {
		return delegate.ready();
	}

	public boolean markSupported() {
		return true;
	}

	public void mark(int readAheadLimit) throws IOException {
		delegate.mark(readAheadLimit);
		mark.assign(current);
	}

	/**
	 * Reset to the last mark. 
	 */
	public void reset() throws IOException {
		delegate.reset();
		current.assign(mark);
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
		if(offset > fileSize) throw new IndexOutOfBoundsException("Past EOF");
		
		if(offset == current.offset) {
			// Do nothing, we're already there
		} else if(offset > current.offset) {
			// Scan ahead
			skip(offset-current.offset);
		} else if(offset == mark.offset) {
			// Jump back to the mark
			reset();
		} else if(offset > mark.offset) {
			// Save some line/column calculations if we're seeking back within the current line
			int charsBack = current.offset - offset;
			boolean sameLineAsCurrent = charsBack < current.col;
			if(sameLineAsCurrent) {
				// Reposition the reader on the desired character
				delegate.reset();
				delegate.skip(offset-mark.offset);
				
				// Just directly calculate the column number
				current.col -= charsBack;
				current.offset = offset;
			} else {
				// Jump back to the mark, then scan ahead to the given offset while recomputing the file offset
				// This is slower than the above
				reset();
				skip(offset - mark.offset);
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
		if(offset.offset == current.offset) {
			// Do nothing, we're already there
		} else if(offset.offset == mark.offset) {
			// Jump back to the mark
			reset();
		} else if(offset.offset > current.offset){
			skip(offset.offset - current.offset);
		} else if(offset.offset > mark.offset){
			delegate.reset();
			delegate.skip(offset.offset-mark.offset);
			current.assign(offset);
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
		if(filePos.offset == current.offset) {
			// Do nothing, we're already there
		} else if(filePos.offset == mark.offset) {
			// Jump back to the mark
			reset();
		} else if(filePos.offset > current.offset) {
			skip(filePos.offset - current.offset);
		} else if(filePos.offset > mark.offset){
			delegate.reset();
			delegate.skip(filePos.offset-mark.offset);
			current.assign(filePos);
		} else {
			throw new IndexOutOfBoundsException("Cannot scan back past the last mark.");
		}
	}
	
	/**
	 * Seek to the end of the given token.  The token is assumed to have come from
	 * this file and to have a correct end line and column in it.
	 * <p>
	 * After this call, the file position will be set to read the character
	 * immediately following the provided token.
	 * 
	 * @param token Token to use as a reference.
	 * @throws IOException If bytes had to be read from the the file and an errors occurs while doing so
	 */
	public void seekPast(Token token) throws IOException {
		this.seek(token.getFileRange().getEnd());
	}
	
	/**
	 * Get the current offset into the source, in characters.  This starts at 0.
	 */
	public int getCurrentOffset() {
		return current.offset;
	}

	/**
	 * Get the current line number in the source.  This starts at 1.
	 */
	public int getCurrentLineNumber() {
		return current.line;
	}

	/**
	 * Get the current column number in the source.  This starts at 1.
	 */
	public int getCurrentColumnNumber() {
		return current.col;
	}
	
	public void close() throws IOException {
		delegate.close();
	}

	public int remaining() {
		return fileSize - current.offset;
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
		return p.matcher(toCharSequence());
	}
	
	/**
	 * Look ahead for the given regular expression; return a Token
	 * if the pattern matches.
	 *
	 * One should seek to the desired parse position before calling this;
	 * it leaves the file position after the token that was consumed (if any)
	 * or in the original position if the match fails.
	 * This will not match an empty string even if the regular expression
	 * would allow it.
	 * 
	 * @param ignoredTokens Tokens that were ignored as comments/whitespace immediately before this one 
	 */
	public Token checkNextToken(Pattern re, String ignored) throws IOException {
		FilePos start = getFilePos();
		Matcher m = matcher(re);
		if(m.lookingAt() && m.end() > m.start()) {
			// Position just at the end of the token that was matched
			String text = m.group();
			seek(start);
			skip(m.end());
			return new Token(getFileRange(start), text, ignored);
		}
		seek(start);
		return null;
	}
	
	/**
	 * Get the current file position as a FilePos instance.
	 */
	public FilePos getFilePos() {
		return current.toFilePos();
	}

	/**
	 * Create a new parser reader.
	 * 
	 * @param delegate Reader to delegate to.  If it doesn't support marks, this will 
	 *                 wrap it in a buffered reader capable of buffering the entire file.
	 *                 If it does support marks, it should never invalidate the mark
	 *                 no matter how many characters are read.  This calls mark() immediately
	 *                 to allow "seeking" within the stream.
	 * @param filename Name of the file to report in the file location information attached to tokens
	 * @param fileSize Total length of the file
	 * @throws IOException If there's a problem getting the length of the file
	 */
	public ParserReader(Reader delegate, String filename, int fileSize) throws IOException {
		if(!delegate.markSupported()) {
			delegate = new BufferedReader(delegate, fileSize);
		}
		this.delegate = delegate;
		this.filename = filename;
		this.fileSize = fileSize;
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
		this.filename = url.toString();
		this.fileSize = c.getContentLength();
		if(this.fileSize == -1)
			throw new UnsupportedOperationException("Don't yet support reading a file where we don't know the file size in advance.");
		String contentEncoding = c.getContentEncoding();
		if(contentEncoding == null) contentEncoding = Charset.defaultCharset().name();
		this.delegate = new BufferedReader(new InputStreamReader(c.getInputStream(), contentEncoding), this.fileSize);
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
			return new ParserReader(new StringReader(body), filename, body.length());
		} catch (IOException e) {
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
		return new FileRange(filename, from, current.toFilePos());
	}

	public String getFilename() {
		return filename;
	}

	/** Return a zero-length token at the current file position */
	public Token markerToken() {
		return new Token(getFileRange(getFilePos()), "");
	}

	/**
	 * Read any input matching the given terminals and return it as a string.
	 * <p> 
	 * The input is positioned immediately after the last character matched.
	 * <p>
	 * Returns null if the regular expression doesn't match.
	 */
	public String consume(Pattern re) throws IOException {
		FilePos start = getFilePos();
		Matcher m = matcher(re);
		if(m.lookingAt() && m.end() > m.start()) {
			// Position just at the end of the token that was matched
			String text = m.group();
			seek(start);
			skip(m.end());
			return text;
		}
		seek(start);
		return "";
	}

	public String readString(int chars) throws IOException {
		char[] buf = new char[chars];
		int remaining = chars;
		while(remaining > 0) {
			int didRead = read(buf);
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
		FilePos start = getFilePos();
		final int len = expected.length();
		for(int i=0; i < len; i++) {
			int ch = read();
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
		FilePos start = getFilePos();
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
		FilePos pos = getFilePos();
		return new FileRange(getFilename(), pos, pos);
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
		StringBuffer sb = new StringBuffer();
		for(;;) {
			int ch = read();
			if(ch == '\n' || ch == -1) {
				return sb.toString();
			}
			sb.append((char)ch);
		}
	}

	public String positionInLineAsString() throws IOException {
		FilePos start = getFilePos();
		StringBuffer sb = new StringBuffer();
		sb.append(readLineContaining(start)).append("\n");
		for(int i=0; i <= readLineContaining(start).length(); i++) {
			sb.append((i+1)==start.column?'^':' ');
		}
		sb.append('\n');
		seek(start);
		return sb.toString();
	}

	/**
	 * Create a FileRange that is a subRange of the given token's range.  This makes
	 * sure that line numbers are calculated appropriately.
	 * 
	 * @param token Token to use for the range
	 * @param startOffset Offset from the token's start to start the range in
	 * @param endOffset Offset from the token's start to end the range in (exclusive)
	 * @return A new FileRange
	 * @throws IOException 
	 */
	public FileRange subRange(Token token, int startOffset, int endOffset) throws IOException {
		return subRange(token.getFileRange(), startOffset, endOffset);
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
		if(!range.getFilename().equals(getFilename())) throw new IllegalArgumentException("Given range is not for this parser's file");
		if(endOffset < startOffset) throw new IllegalArgumentException("End offset must be greater than start offset");
		
		FilePos curPos = getFilePos();
		try {
			seek(range.getStart());
			skip(startOffset);
			FilePos rangeStart = getFilePos();
			skip(endOffset - startOffset);
			return getFileRange(rangeStart);
		} finally {
			seek(curPos);
		}
	}

	/**
	 * Read a token starting at the given file position up to the current position.  Useful if you have been parsing some stuff
	 * and want to bundle up everything you parsed into a new Token.
	 * 
	 * @param tokenStartPos Start position of the token to read
	 * @param ignored Any whitespace ignored just prior to that token
	 * @return A new Token taken from that range
	 * @throws IOException If there's a problem reading the data
	 */
	public Token readTokenFrom(FilePos tokenStartPos, String ignored) throws IOException {
		int endOffset = current.offset;
		seek(tokenStartPos);
		String text = readString(endOffset - tokenStartPos.offset);
		FileRange range = getFileRange(tokenStartPos);
		return new Token(range, text, ignored);
	}
	
	
	
}
