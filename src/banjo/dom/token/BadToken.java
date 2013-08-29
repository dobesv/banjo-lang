package banjo.dom.token;


public class BadToken implements Token {
	private final String text;
	private final String message;

	public BadToken(String text, String message) {
		super();
		this.text = text;
		this.message = message;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append(this.text);
	}

	@Override
	public String toSource() {
		return this.text;
	}

	public String getText() {
		return this.text;
	}

	public String getMessage() {
		return this.message;
	}


}
