package banjo.expr.core.test;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import banjo.expr.core.BaseCoreExprVisitor;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.ScopedExpr;
import banjo.expr.core.SourceExprToCoreExpr;
import banjo.expr.core.TestAndExampleGatherer;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;
import junit.framework.AssertionFailedError;

@RunWith(Parameterized.class)
public class TestCoreLibraryTests extends BaseExprTest {
	private CoreExpr withoutScope;

	public TestCoreLibraryTests(CoreExpr expr, CoreExpr withoutScope) {
		this.expr = expr;
		this.withoutScope = withoutScope;
	}

	@Override
	public CoreExpr getAst() {
		return expr;
	}

	private static List<CoreExpr> allTests;

	public static List<CoreExpr> getAllTests() {
		if (allTests == null) {
			allTests = findAllTests();
		}
		return allTests;
	}

	public static List<CoreExpr> findAllTests() {
		List<Path> paths = SourceExprToCoreExpr
				.projectSourcePathsForFile(Paths.get("src/test/banjo/").toAbsolutePath());
		CoreExpr ast = SourceExprToCoreExpr.INSTANCE.loadFromDirectories(paths);
		return TestAndExampleGatherer.findTests(ast).toList();
	}

	@Parameters(name = "{1}")
	public static List<Object[]> parameters() {
		return findAllTests().map(x -> new Object[] { x, stripScope(x) });
	}

	static CoreExpr stripScope(CoreExpr x) {
		return x.acceptVisitor(new BaseCoreExprVisitor<CoreExpr>() {
			@Override
			public CoreExpr fallback() {
				return x;
			}

			@Override
			public CoreExpr scoped(ScopedExpr projection) {
				return projection.getBody();
			}
		});
	}

	public static void setStackTrace(CoreExpr x, final AssertionFailedError afe) {
		afe.setStackTrace(new StackTraceElement[] { new StackTraceElement("<examples>", x.toSource(),
				x.getRanges().toStream().toOption().map(sfr -> sfr.getSourceFile().toString()).toNull(),
				x.getRanges().toStream().toOption().map(sfr -> sfr.getStartLine()).orSome(-1)) });
	}

	@Override
	public String exprSource() {
		return withoutScope.toSource();
	}

	@Override
	public Set<SourceFileRange> exprRanges() {
		return withoutScope.getRanges();
	}
}
