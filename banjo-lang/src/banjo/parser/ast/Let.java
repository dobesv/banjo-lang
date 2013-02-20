package banjo.parser.ast;

import java.io.IOException;
import java.text.ParseException;
import java.util.Collection;

import banjo.parser.BanjoParser;
import banjo.parser.BanjoParser.BanjoParseException;
import banjo.parser.util.FilePos;
import banjo.parser.util.ParserReader;
import banjo.parser.util.Token;

public class Let extends Expr {
	Token identifier;
	Expr value;
	Expr body;
	
	public Let(Token identifier, Expr value, Expr body) {
		super();
		this.identifier = identifier;
		this.value = value;
		this.body = body;
	}

	/**
	 * Look at the input stream and determine if it is supposed to be a "let" expression.  If so, parse it and
	 * return a new Let.  If parsing fails after we've decided this is a "let" expression, throw a parse error.  If
	 * this does not appear to be a let expression, rewind the input and return null.
	 * 
	 * let = ID "=" Expr (";"|nl) Body
	 */
	public static Let parseLet(ParserReader in, Collection<BanjoParseException> errors) throws IOException, ParseException {
		FilePos startPos = in.getFilePos();
		
		Token identifier = BanjoParser.parseID(in);
		if(identifier == null)
			return null;
		
		// TODO Functions
		
		if(!BanjoParser.checkOperator(in, "=")) {
			in.seek(startPos);
			return null;
		}
		
		Expr value = Expr.parseAnyExpr(in, Precedence.ASSIGNMENT, errors);
		if(!BanjoParser.checkSeparatorOrNewline(in, ";", identifier.getFileRange().getStart().col, errors)) {
			// Variable value should be followed by semicolon or a newly indented line
			throw new BanjoParser.ExpectedSemiColonOrNewline(in.getFilePosAsRange());
		}
		Expr body = Expr.parseAnyExpr(in, Precedence.ASSIGNMENT, errors);
		
		return new Let(identifier, value, body);
	}
}
