package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import fj.data.List;
import banjo.dom.source.Operator;
import banjo.dom.source.Operator.Position;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.parser.util.AbstractCachedHashCode;
import banjo.parser.util.ListUtil;
import banjo.parser.util.SourceFileRange;

public class Method extends AbstractCachedHashCode implements Comparable<Method> {
	public static class SignaturePart implements Comparable<SignaturePart> {
		private final Key key;
		private final fj.data.List<MethodFormalArgument> arguments;
		
		public SignaturePart(Key key, List<MethodFormalArgument> arguments) {
			super();
			this.key = key;
			this.arguments = arguments;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + arguments.hashCode();
			result = prime * result + key.hashCode();
			return result;
		}

		@Override
		public boolean equals(@Nullable Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			SignaturePart other = (SignaturePart) obj;
			if (!arguments.equals(other.arguments))
				return false;
			if (!key.equals(other.key))
				return false;
			return true;
		}

		public Key getKey() {
			return key;
		}

		public fj.data.List<MethodFormalArgument> getArguments() {
			return arguments;
		}
		
		public void formalArgListToSource(final StringBuffer sb) {
			sb.append('(');
			boolean first = true;
			for(final MethodFormalArgument arg : this.arguments) {
				if(first) first = false;
				else sb.append(", ");
				arg.toSource(sb);
			}
			sb.append(')');
		}

		@Override
		public int compareTo(@Nullable SignaturePart o) {
			if(o == null) return -1;
			int cmp = key.compareTo(o.key);
			if(cmp == 0) cmp = ListUtil.compare(arguments, o.arguments);
			return cmp;
		}
		
	}
	
	private final Key selfName;
	private final fj.data.List<SignaturePart> parts;
	private final CoreExpr guarantee;
	private final CoreExpr body;
	private final SourceFileRange sourceFileRange;

	public static final CoreExpr NO_GUARANTEE = MethodFormalArgument.NO_ASSERTION;
	public static final Key APPLY_FUNCTION_METHOD_NAME = new Identifier(SourceFileRange.SYNTHETIC, Operator.CALL.getOp());
	public static final Key LOOKUP_METHOD_NAME = new Identifier(SourceFileRange.SYNTHETIC, Operator.LOOKUP.getOp());
	public static final Key NO_SELF_NAME = new Identifier(SourceFileRange.SYNTHETIC, "__no_self_name__");
	public static final fj.data.List<MethodFormalArgument> NO_ARGS = fj.data.List.<MethodFormalArgument>nil();

	public Method(SourceFileRange sfr, Key selfName, fj.data.List<SignaturePart> parts, CoreExpr guarantee, CoreExpr body) {
		super(calcHash(selfName, parts, guarantee, body, sfr));
		this.selfName = selfName;
		this.parts = parts;
		this.guarantee = guarantee;
		this.body = body;
		this.sourceFileRange = sfr;
	}

	private static int calcHash(Key selfName, fj.data.List<SignaturePart> parts,
			CoreExpr guarantee, CoreExpr body, SourceFileRange sfr) {
		final int prime = 31;
		int result = 1;
		result = prime * result + selfName.hashCode();
		result = prime * result + parts.hashCode();
		result = prime * result + guarantee.hashCode();
		result = prime * result + body.hashCode();
		result = prime * result + sfr.hashCode();
		return result;
	}

	@SuppressWarnings("null")
	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		toSource(sb);
		return sb.toString();
	}

	public void toSource(final StringBuffer sb) {
		// Check for brackets, apply

		final boolean hasSelfName = this.hasSelfName();
		final Operator operator = !hasSelfName ? null 
				: this.parts.length() == 1
					? this.parts.head().getKey().equals(APPLY_FUNCTION_METHOD_NAME) ? Operator.CALL
					: this.parts.head().getKey().equals(LOOKUP_METHOD_NAME) ? Operator.LOOKUP
					: this.parts.head().getArguments().length() == 1  ? Operator.fromMethodName(this.parts.head().getKey().getKeyString(), true) 
					: this.parts.head().getArguments().length() == 0  ? Operator.fromMethodName(this.parts.head().getKey().getKeyString(), false) 
					: null 
				: null;
		if(operator != null && operator.isInfix()) {
			sb.append('(');
			List<MethodFormalArgument> args = this.parts.head().getArguments();
			if(operator.isParen()) {
				this.selfName.toSource(sb);
				sb.append(operator.getParenType().getStartChar());
				boolean first = true;
				for(final MethodFormalArgument arg : args) {
					if(first) first = false;
					else sb.append(", ");
					arg.toSource(sb);
				}
				sb.append(operator.getParenType().getEndChar());
			} else if(operator.isSelfOnRightMethodOperator()) {
				args.head().toSource(sb);
				sb.append(' ');
				operator.toSource(sb);
				sb.append(' ');
				this.selfName.toSource(sb);
			} else {
				this.selfName.toSource(sb);
				sb.append(' ');
				operator.toSource(sb);
				sb.append(' ');
				args.head().toSource(sb);
			}
			sb.append(')');
		} else if(operator != null && (operator == Operator.CALL || operator == Operator.BRACKETS)) {
			sb.append(operator.getParenType().getStartChar());
			boolean first = true;
			for(final MethodFormalArgument arg : this.parts.head().getArguments()) {
				if(first) first = false;
				else sb.append(", ");
				arg.toSource(sb);
			}
			sb.append(operator.getParenType().getEndChar());
		} else if(operator != null && operator.isPrefix()) {
			sb.append('(');
			operator.toSource(sb);
			this.selfName.toSource(sb);
			sb.append(')');
		} else if(operator != null && operator.isSuffix()) {
			sb.append('(');
			this.selfName.toSource(sb);
			operator.toSource(sb);
			sb.append(')');
		} else {
			if(hasSelfName) {
				this.selfName.toSource(sb);
				sb.append('.');
			} else if(this.parts.length() == 1 && this.parts.head().getKey().equals(this.body) && this.parts.head().getArguments().isEmpty() && this.guarantee.equals(NO_GUARANTEE)) {
				this.body.toSource(sb);
				return;
			}
			
			boolean prevPartNoArgs = false;
			boolean firstPart = true;
			for(SignaturePart part : parts) {
				if(firstPart) firstPart = false;
				else if(prevPartNoArgs) sb.append("() ");
				else sb.append(" ");
				part.key.toSource(sb);
				if(!part.arguments.isEmpty()) {
					part.formalArgListToSource(sb);
					prevPartNoArgs = false;
				} else {
					prevPartNoArgs = true;
				}
			}
		}
		if(hasGuarantee()) {
			sb.append(' ');
			Operator.MEMBER_OF.toSource(sb);
			sb.append(' ');
			this.guarantee.toSource(sb, Precedence.COLON);
		}
		sb.append(' ');
		Operator.ASSIGNMENT.toSource(sb);
		sb.append(' ');
		this.body.toSource(sb, Precedence.COLON);
	}

	public fj.data.List<SignaturePart> getParts() {
		return parts;
	}

	public CoreExpr getGuarantee() {
		return this.guarantee;
	}

	public CoreExpr getBody() {
		return this.body;
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof Method))
			return false;
		final Method other = (Method) obj;
		if (!this.parts.equals(other.parts))
			return false;
		if (!this.body.equals(other.body))
			return false;
		if (!this.guarantee.equals(other.guarantee))
			return false;
		if (!this.selfName.equals(other.selfName))
			return false;
		if (!this.sourceFileRange.equals(other.sourceFileRange))
			return false;
		return true;
	}

	@Override
	public int compareTo(@Nullable Method other) {
		if(other == null) return -1;
		int cmp = ListUtil.compare(this.parts, other.parts);
		if(cmp == 0) cmp = this.guarantee.compareTo(other.guarantee);
		if(cmp == 0) cmp = this.selfName.compareTo(other.selfName);
		if(cmp == 0) cmp = this.body.compareTo(other.body);
		if(cmp == 0) cmp = this.sourceFileRange.compareTo(other.sourceFileRange);
		return cmp;
	}

	public boolean hasSelfName() {
		return !this.selfName.equals(NO_SELF_NAME);
	}

	public Key getSelfName() {
		return this.selfName;
	}

	public boolean hasGuarantee() {
		return !this.guarantee.equals(NO_GUARANTEE);
	}

	/**
	 * True if this method could have been created using the lambda syntax (x,...) -> y
	 */
	public boolean isSimpleApplyMethod() {
		return !hasGuarantee() && this.parts.length() == 1 && this.parts.head().getKey().equals(APPLY_FUNCTION_METHOD_NAME);
	}

	public SourceFileRange getSourceFileRange() {
		return this.sourceFileRange;
	}



}