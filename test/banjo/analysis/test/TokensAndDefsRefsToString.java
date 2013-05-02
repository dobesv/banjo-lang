package banjo.analysis.test;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;

import banjo.analysis.DefInfo;
import banjo.analysis.DefRefScanner;
import banjo.analysis.DefRefTokenVisitor;
import banjo.dom.core.CoreExpr;
import banjo.dom.test.ParseTestUtils;
import banjo.dom.token.Identifier;
import banjo.parser.test.TokensToString;
import banjo.parser.util.ParserReader;

public class TokensAndDefsRefsToString extends TokensToString implements DefRefTokenVisitor<String> {

	public TokensAndDefsRefsToString(int rangeStart, int rangeEnd) {
		super(rangeStart, rangeEnd);
	}

	public static void testScanner(@NonNull String src, final int rangeStart, final int rangeEnd, String ... expectedTokens) {
		DefRefScanner scanner = new DefRefScanner();
		CoreExpr ast = ParseTestUtils.test(src, null, CoreExpr.class);
		TokensAndDefsRefsToString testVisitor = new TokensAndDefsRefsToString(rangeStart, rangeEnd);
		ParserReader in = ParserReader.fromString("", src);
		ArrayList<String> foundTokens = new ArrayList<String>(expectedTokens.length);
		while(!testVisitor.done) {
			foundTokens.add(scanner.nextToken(in, ast, testVisitor));
		}
		assertEquals(Arrays.asList(expectedTokens).toString(), foundTokens.toString());
	}

	@Override
	@Nullable
	public String visitIdentifierDef(@NonNull Identifier identifier, @NonNull DefInfo def) {
		return token("def "+def.getType()+" "+identifier, range);
	}

	@Override
	@Nullable
	public String visitIdentifierRef(@NonNull Identifier identifier, @NonNull DefInfo def) {
		// TODO Auto-generated method stub
		return token("ref "+def.getType()+" "+identifier, range);
	}

	
}
