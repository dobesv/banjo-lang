package banjo.analysis.test;

import static banjo.parser.util.Check.nonNull;
import static org.junit.Assert.assertEquals;

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
import banjo.parser.util.FileRange;
import banjo.parser.util.ParserReader;

public class TokensAndDefsRefsToString extends TokensToString implements DefRefTokenVisitor<String> {

	final ParserReader in;
	public TokensAndDefsRefsToString(int rangeStart, int rangeEnd, ParserReader in) {
		super(rangeStart, rangeEnd);
		this.in = in;
	}

	public static void testScanner(@NonNull String src, final int rangeStart, final int rangeEnd, String ... expectedTokens) {
		final DefRefScanner scanner = new DefRefScanner();
		final CoreExpr ast = ParseTestUtils.test(src, null, CoreExpr.class);
		final TokensAndDefsRefsToString testVisitor = new TokensAndDefsRefsToString(rangeStart, rangeEnd, ParserReader.fromSubstring("<src>", src, rangeStart, rangeEnd));
		final ParserReader in = ParserReader.fromString("", src);
		final ArrayList<String> foundTokens = new ArrayList<String>(expectedTokens.length);
		while(!testVisitor.done) {
			foundTokens.add(scanner.nextToken(in, nonNull(ast), testVisitor));
		}
		assertEquals(Arrays.asList(expectedTokens).toString(), foundTokens.toString());
	}

	@Override
	@Nullable
	public String visitIdentifierDef(@NonNull FileRange range, @NonNull Identifier identifier, @NonNull DefInfo def) {
		return token("def "+def.getType()+" "+identifier, range);
	}

	@Override
	@Nullable
	public String visitIdentifierRef(@NonNull FileRange range, @NonNull Identifier identifier, @NonNull DefInfo def) {
		return token("ref "+def.getType()+" "+identifier, range);
	}


}
