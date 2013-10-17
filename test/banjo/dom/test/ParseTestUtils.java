package banjo.dom.test;

import static banjo.parser.util.Check.nonNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;

import banjo.desugar.BanjoDesugarer;
import banjo.desugar.BanjoDesugarer.DesugarResult;
import banjo.dom.BadExpr;
import banjo.dom.Expr;
import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.CoreExpr;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.NumberLiteral;
import banjo.parser.BanjoParser;
import banjo.parser.BanjoParser.ExtSourceExpr;
import banjo.parser.util.FileRange;
import banjo.parser.util.ParserReader;
import banjo.parser.util.SourceMap;
import fj.P2;
import fj.data.Set;

@NonNullByDefault(false)
public class ParseTestUtils {

	public ParseTestUtils() {
		// TODO Auto-generated constructor stub
	}

	public static <T extends Expr> T test(String source, int expectedErrors, Class<? extends BadExpr> expectedErrorClass, Class<T> expectedClass, String normalizedSource) {
		final BanjoParser parser = new BanjoParser();
		return test(source, expectedErrors, expectedErrorClass, expectedClass,
				normalizedSource, parser);
	}

	public static <T extends Expr> T test(String source, int expectedErrors,
			Class<? extends BadExpr> expectedErrorClass,
			Class<T> expectedClass, String normalizedSource, BanjoParser parser)
					throws Error {
		System.out.println("Source input:\n  "+source.replace("\n", "\n  "));
		SourceMap sourceMaps = new SourceMap();
		SourceExpr parseTree;
		try {
			final ExtSourceExpr parseResult = parser.parse(source);
			parseTree = parseResult.getExpr();
			sourceMaps = parseResult.getSourceMaps();
		} catch (final IOException e1) {
			throw new Error(e1);
		}
		System.out.println("Parsed:\n  " + parseTree.toSource().replace("\n", "\n  "));
		final ParserReader in = ParserReader.fromString("<source>", source);
		assertTrue(parser.reachedEof());
		int errCount = parseErrors(expectedErrorClass, sourceMaps, in);
		if(errCount == 0) {
			final BanjoDesugarer desugarer = new BanjoDesugarer(sourceMaps);
			final DesugarResult<CoreExpr> desugarResult = desugarer.desugar(parseTree);
			final CoreExpr ast = desugarResult.getValue();
			System.out.println("Desugared:\n  " + ast.toSource().replace("\n", "\n  "));

			errCount = desugarErrors(expectedErrorClass, desugarResult, in);
			if(normalizedSource != null)
				assertEquals(normalizedSource, ast.toSource());
			if(errCount == 0 && expectedClass != null) {
				assertTrue("Expecting an instance of "+expectedClass.getName()+" but got "+ast.getClass().getName(), expectedClass.isInstance(ast));
				assertNotNull(ast);
				return expectedClass.cast(ast);
			}

		}
		assertEquals("Wrong number of errors found", expectedErrors, errCount);
		return null;
	}

	public static int parseErrors(Class<? extends BadExpr> expectedClass,
			SourceMap sourceMaps,
			ParserReader in) throws Error {
		int count = 0;
		BadExpr first = null;
		for(final P2<SourceExpr, Set<FileRange>> p : sourceMaps) {
			final SourceExpr n = p._1();
			if(n instanceof BadExpr) {
				final BadExpr e = (BadExpr) n;
				if(count == 0) {
					System.out.println("Parse Errors:");
					first = e;
				}
				final Set<FileRange> locs = p._2();
				for(final FileRange range : locs) {
					System.out.println("  "+range+": "+e.getMessage());
					count ++;
				}
			}
		}
		if(expectedClass != null && first != null)
			assertEquals(expectedClass, first.getClass());
		return count;
	}

	private static int desugarErrors(Class<? extends BadExpr> expectedClass,
			DesugarResult<CoreExpr> ds,
			ParserReader in) throws Error {
		int count = 0;
		BadExpr first = null;
		for(final P2<CoreExpr, Set<SourceExpr>> p : ds.getDesugarMap().getCoreExprToSourceExpr()) {
			final CoreExpr n = p._1();
			if(n instanceof BadExpr) {
				final BadExpr e = (BadExpr) n;
				if(count == 0) {
					System.out.println("Desugar Errors:");
					first = e;
				}
				for(final SourceExpr sourceExpr : p._2()) {
					for(final FileRange range : ds.getSourceMaps().get(nonNull(sourceExpr))) {
						System.out.println("  "+range+": "+e.getMessage());
						count ++;
					}
				}
			}
		}
		if(expectedClass != null && first != null)
			assertEquals(expectedClass, first.getClass());
		return count;
	}

	public static void test(String source, String expectedSource) {
		test(source, expectedSource, null);

	}
	public static <T extends CoreExpr> T test(String source, Class<T> expectedClass) {
		return test(source, source, expectedClass);
	}

	public static <T extends CoreExpr> T test(String source, String expectedSource, Class<T> expectedClass) {
		return test(source, 0, null, expectedClass, expectedSource);
	}

	public static void assertIsNumberLiteralWithValue(final long value, CoreExpr e) {
		e.acceptVisitor(new BaseCoreExprVisitor<Void>() {
			@Override
			@Nullable
			public Void numberLiteral(@NonNull NumberLiteral n) {
				assertEquals(n.getNumber().longValue(), value);
				return null;
			}
			@Override
			public Void fallback(@NonNull CoreExpr unsupported) {
				fail("Not a number literal: "+unsupported);
				return null;
			}
		});
	}


}
