package banjo.analysis;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.token.Identifier;
import banjo.dom.token.TokenVisitor;

/**
 * Accept tokens.  Unlike the standard token visitor, some tokens
 * are provided with def/ref information, if available.  If the
 * definition information is not available, the version of the method
 * without that information will be called.
 */
public interface DefRefTokenVisitor<T> extends TokenVisitor<T> {

	@Nullable T visitIdentifierDef(Identifier identifier, DefInfo def);
	@Nullable T visitIdentifierRef(Identifier identifier, DefInfo def);

}
