package banjo.dom.core;

import fj.data.List;
import banjo.dom.token.BadIdentifier;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.parser.util.SourceFileRange;

public interface CoreExprVisitor<T> extends ExprVisitor<T> {
	T stringLiteral(StringLiteral stringLiteral);
	T numberLiteral(NumberLiteral numberLiteral);
	T identifier(Identifier identifier);
	T operator(OperatorRef operatorRef);
	T call(Call call);
	T objectLiteral(ObjectLiteral objectLiteral);
	T listLiteral(ListLiteral listLiteral);
	T badIdentifier(BadIdentifier badIdentifier);
	T inspect(Inspect inspect);
	T extend(Extend extend);
	T method(List<SourceFileRange> sourceFileRanges, List<Key> selfArg,
			List<Key> nameParts, List<List<List<Key>>> argumentLists,
			CoreExpr body);

}
