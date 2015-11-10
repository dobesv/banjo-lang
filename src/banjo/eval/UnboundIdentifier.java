package banjo.eval;


public class UnboundIdentifier extends Fail {
	public final String id;
	
	public UnboundIdentifier(String id) {
        this.id = id;
    }
	
	@Override
	public String getMessage() {
		return "The name "+id+" is not defined / bound here";
	}
}