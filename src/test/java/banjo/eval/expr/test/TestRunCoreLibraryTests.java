package banjo.eval.expr.test;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import banjo.expr.core.CoreExpr;
import banjo.expr.core.CoreExprFactory;
import banjo.expr.core.Let;
import banjo.expr.core.TestAndExampleGatherer;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;
import junit.framework.AssertionFailedError;

@RunWith(Parameterized.class)
public class TestRunCoreLibraryTests extends BaseExprTest {
	private CoreExpr withoutScope;

	public TestRunCoreLibraryTests(CoreExpr expr, CoreExpr withoutScope) {
	    this.expr = expr;
	    this.withoutScope = withoutScope;
    }

	@Override
	public CoreExpr getAst() {
		return expr;
	}
	
	private static List<CoreExpr> allTests;

	public static List<CoreExpr> getAllTests() {
		if(allTests == null) {
			allTests = findAllTests();
		}
		return allTests;
	}

	public static List<CoreExpr> findAllTests() {
        List<Path> paths = CoreExprFactory.projectSourcePathsForFile(Paths.get("src/test/banjo/").toAbsolutePath());
        CoreExpr ast = CoreExprFactory.INSTANCE.loadFromDirectories(paths);
        return TestAndExampleGatherer.findTests(ast);
	}

	@Parameters(name="{1}")
	public static List<Object[]> parameters() {
		return findAllTests().map(x -> new Object[]{x, stripScope(x)});
	}

	static CoreExpr stripScope(CoreExpr x) {
		while(x instanceof Let) {
			x = ((Let)x).body;
		}
		return x;
	}
	public static void setStackTrace(CoreExpr x, final AssertionFailedError afe) {
	    afe.setStackTrace(new StackTraceElement[] {
	    	new StackTraceElement("<examples>",
	    		x.toSource(),
	    		x.getSourceFileRanges().toStream().toOption().map(sfr -> sfr.getSourceFile().toString()).toNull(),
	    		x.getSourceFileRanges().toStream().toOption().map(sfr -> sfr.getStartLine()).orSome(-1))
	    });
    }
	
	@Override
	public String exprSource() {
		return withoutScope.toSource();
	}
	
	@Override
	public Set<SourceFileRange> exprRanges() {
		return withoutScope.getSourceFileRanges();
	}
}
