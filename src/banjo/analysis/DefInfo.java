package banjo.analysis;

import banjo.dom.token.Key;

public class DefInfo {
	private final Key nameToken;
	private final int sourceOffset;
	private final DefType type;
	private final int scopeDepth;

	public DefInfo(Key nameToken, int sourceOffset, final DefType type, int scopeDepth) {
		super();
		this.nameToken = nameToken;
		this.sourceOffset = sourceOffset;
		this.type = type;
		this.scopeDepth = scopeDepth;
	}

	public int getScopeDepth() {
		return this.scopeDepth;
	}

	public Key getNameToken() {
		return this.nameToken;
	}

	public DefType getType() {
		return this.type;
	}

	public int getSourceOffset() {
		return this.sourceOffset;
	}

}
