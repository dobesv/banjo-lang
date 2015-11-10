package banjo.eval;

import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class UnresolvedCodeError extends FailWithMessage {
	public final Set<SourceFileRange> sourceFileRanges;
	
	public UnresolvedCodeError(String message,
            Set<SourceFileRange> sourceFileRanges) {
		super(message);
	    this.sourceFileRanges = sourceFileRanges;
    }

	@Override
	public String getMessage() {
	    if(!sourceFileRanges.isEmpty()) return sourceFileRanges.toStream().head()+": "+message;
		return message;
	}

}
