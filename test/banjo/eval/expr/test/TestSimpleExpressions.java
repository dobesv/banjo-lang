package banjo.eval.expr.test;

import static org.junit.Assert.*;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import banjo.eval.Fail;
import banjo.expr.core.CoreErrorGatherer;
import banjo.expr.core.CoreExpr;
import fj.data.Stream;

@RunWith(Parameterized.class)
public class TestSimpleExpressions extends BaseSourceExprTest {

	public TestSimpleExpressions(String src) {
	    super(src);
    }

	@Parameters(name="{0}")
	public static Stream<Object[]> parameters() {
		return Stream.stream(
			"true",
			"true == true",
			"!(true == false)",
			"! true == ! true",
			"!true == false",
			"true == ! false",
			"true && true",
			"true && !false",
			"false || true",
			"! false",
			"false == false",
			"!(false == true)",
			"false == !true",
			"! false == ! false",
			"0 == 0", "-0 == 0", "0 == -0",
			"0.0 == 0", "0 == 0.0",
			"[] == []",
			"[true] == [true]",
			"[false] == [false]",
			"[false] != []",
			"[] != [false]",
			"[].is empty",
			"0 . is zero",
			"0 == 0.0",
			"0.0 == 0",
			"4 / 2 * 2 == 4",
			"4 * 0.5 * 2 == 4",
			"4 * 0.5 == 4 / 2",
			"[1, 2, 3, 4, 5].slice(2, 4) == [3, 4]",
			"[1, 2, 3, 4, 5].slice(2, 2) == []",
			"[1, 2, 3, 4, 5, 6].slice(2, -1) == [3, 4, 5]",
			"(not = (x -> !x), not not = not ; not) ⇒ not not(true)",
			"(not = (x -> !x), not not = not ; not, not not not = not not ; not) ⇒ not not not(true) == false"
		).append(Stream.join(Stream.range(1, 10).map(i -> Stream.stream(
				String.valueOf(i)+" == "+String.valueOf(i),
				String.valueOf(-i)+" == "+String.valueOf(-i),
				String.valueOf(-i)+" < "+String.valueOf(i),
				String.valueOf(-i)+" <= "+String.valueOf(i),
				String.valueOf(-i)+" != "+String.valueOf(i),
				String.valueOf(i)+" >= "+String.valueOf(-i),
				String.valueOf(i)+" > "+String.valueOf(-i),
				String.valueOf(i)+" != "+String.valueOf(-i)
				))))
		.map(s -> new Object[] {s});
	}
}
