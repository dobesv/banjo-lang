package banjo.parser.util;

public class ParserStringUtils {

	/**
	 * Quote a string for output to the user (in a message / error of some sort)
	 */
	public static String friendlyQuote(String src) {
		String lq;
		String rq;
		if(src.indexOf('"') == -1) {
			lq = rq = "\"";
		} else if(src.indexOf('\'') == -1) {
			lq = rq = "'";
		} else if(src.indexOf("``") == -1 && src.indexOf("''") == -1) {
			lq = "``";
			rq = "''";
		} else if(src.indexOf("[") == -1 && src.indexOf("]") == -1) {
			lq = "[";
			rq = "]";
		} else {
			lq = rq = "\"";
		}
		return lq + src.replace("\n", "\\n").replace("\r", "\\r") + rq;
	}
}
