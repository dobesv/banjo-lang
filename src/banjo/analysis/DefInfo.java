package banjo.analysis;

import banjo.dom.Key;

public class DefInfo {
	private final Key nameToken;
	private final DefType type;
	private final int scopeDepth;

	public DefInfo(Key nameToken, final DefType type, int scopeDepth) {
		super();
		this.nameToken = nameToken;
		this.type = type;
		this.scopeDepth = scopeDepth;
	}

	public int getScopeDepth() {
		return scopeDepth;
	}
	
	public Key getNameToken() {
		return nameToken;
	}

	public DefType getType() {
		return type;
	}

}
