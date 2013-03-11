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
			Class<T> expectedClass, String expectedSource) {
		BanjoParser parser = new BanjoParser(source);
		Expr parsed;
		try {
			parsed = parser.parseExpr();
		} catch (BanjoParseException e1) {
			throw new Error(e1);
		} catch (IOException e1) {
			throw new Error(e1);
		}
		assertTrue(parser.reachedEof());
		System.out.println(parsed.toSource());
		for(Exception e : parser.getErrors()) {
			System.out.println(e);
		}
		assertEquals(expectedErrors, parser.getErrors().size());
		if(expectedSource != null)
			assertEquals(expectedSource, parsed.toSource());
		
		return expectedClass.cast(parsed);
	}

}
