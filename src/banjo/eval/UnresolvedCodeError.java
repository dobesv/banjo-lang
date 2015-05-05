package banjo.eval;

import banjo.parser.util.SourceFileRange;
import fj.data.List;

public class UnresolvedCodeError extends Fail {
	public final List<SourceFileRange> sourceFileRanges;

	public UnresolvedCodeError(String message,
            List<SourceFileRange> sourceFileRanges) {
	    super(message);
	    this.sourceFileRanges = sourceFileRanges;
    }

	@Override
	public String getMessage() {
	    final String message = super.getMessage();
	    if(sourceFileRanges.isNotEmpty()) return sourceFileRanges.head()+": "+message;
		return message;
	}

}
