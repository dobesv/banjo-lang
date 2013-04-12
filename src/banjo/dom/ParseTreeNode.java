package banjo.dom;

/**
 * Parse tree nodes are output by the parser, and eliminated by the desugaring
 * process ...
 * 
 * @author Dobes
 *
 */
public interface ParseTreeNode extends Expr {

	<T> T acceptVisitor(ParseTreeVisitor<T> visitor);
}
