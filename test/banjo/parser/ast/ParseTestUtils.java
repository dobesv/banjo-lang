package banjo.parser.ast;

import static org.junit.Assert.*;

import java.io.IOException;

import banjo.parser.BanjoParser;
import banjo.parser.errors.BanjoParseException;

public class ParseTestUtils {

	public ParseTestUtils() {
		// TODO Auto-generated constructor stub
	}

	public static <T extends Expr> T testParse(String source, int expectedErrors,
			Class<T> expectedClass, String normalizedSource) {
		System.out.println("<< "+source.replace("\n", "\n<< "));
		BanjoParser parser = new BanjoParser(source);
		Expr parsed;
		try {
			parsed = parser.parseExpr();
		} catch (BanjoParseException e1) {
			throw new Error(e1);
		} catch (IOException e1) {
			throw new Error(e1);
		}
		System.out.println("  >> " + parsed.toSource().replace("\n", "\n  >> "));
		for(Exception e : parser.getErrors()) {
			System.out.println("  !! "+e);
		}
		assertTrue(parser.reachedEof());
		if(expectedErrors==0 && !parser.getErrors().isEmpty())
			throw new Error(parser.getErrors().iterator().next());
		assertEquals("Wrong number of errors found", expectedErrors, parser.getErrors().size());
		if(normalizedSource != null)
			assertEquals(normalizedSource, parsed.toSource());
		
		return expectedClass.cast(parsed);
	}

}
