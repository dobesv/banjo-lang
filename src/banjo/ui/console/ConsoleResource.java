package banjo.ui.console;

import banjo.io.resource.CompositeResource;
import banjo.io.resource.Resource;
import banjo.value.Value;
import fj.Equal;
import fj.data.List;
import static banjo.io.resource.Resource.*;

import java.util.function.Consumer;

public class ConsoleResource extends CompositeResource {

	public ConsoleResource() {
		super(makeSinks());
	}

	static final Equal<Value> stringValueEq = Equal.stringEqual.contramap(v -> v.convertToJava(String.class).left().orValue(""));
	
	public static Resource consolePipe(Consumer<String> target) {
		// TODO: Ignore empty strings ... 
		return Resource.pipeTo(target, String.class).ignoreUndefined().ignoreRepeats(stringValueEq); //.ignoreIf(v -> v.slot("code points").slot("is empty").isTruthy());
	}
	
	public static List<Resource> makeSinks() {
		return List.list(
				Resource.inSlot("console", Resource.inSlot("out", consolePipe(System.out::print))), 
				Resource.inSlot("console", Resource.inSlot("outln", consolePipe(System.out::println))), 
				Resource.inSlot("console", Resource.inSlot("err", consolePipe(System.err::print))),
				Resource.inSlot("console", Resource.inSlot("errln", consolePipe(System.err::println)))
		);
	}	
}
