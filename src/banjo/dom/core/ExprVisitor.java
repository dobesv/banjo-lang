package banjo.dom.core;

import banjo.dom.BadExpr;

public interface ExprVisitor<T> {
	T badExpr(BadExpr badExpr);

}