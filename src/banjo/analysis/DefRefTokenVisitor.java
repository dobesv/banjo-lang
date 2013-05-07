package banjo.analysis;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.token.Identifier;
import banjo.dom.token.TokenVisitor;
import banjo.parser.util.FileRange;

/**
 * Accept tokens.  Unlike the standard token visitor, some tokens
 * are provided with def/ref information, if available.  If the
 * definition information is not available, the version of the method
 * without that information will be called.
 */
public interface DefRefTokenVisitor<T> extends TokenVisitor<T> {

	/**
	 * Visit a definition.  The range specifies the source range of the
	 * token that you would jump to if you jumped to that definition.  The
	 * DefInfo provides information about the type of definition it is.
	 */
	@Nullable T visitIdentifierDef(FileRange range, Identifier identifier, DefInfo def);

	/**
	 * Visit a reference.  The range and identifier describe the reference, whereas
	 * the DefInfo describes the location of the original definition.
	 */
	@Nullable T visitIdentifierRef(FileRange range, Identifier identifier, DefInfo def);

}
