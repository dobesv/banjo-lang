package banjo.ui.javafx;

import banjo.io.resource.CompositeResource;
import banjo.io.resource.Resource;
import fj.data.List;

public class JavaFXResource extends CompositeResource {

	public static List<Resource> makeResources() {
		List<Resource> resources = List.nil();
		//JavaFXState state = new JavaFXState();
		// Need to map javafx.stages on the result value to some list of stages
		// Want to observe when stages are new / removed and bind properties to
		// them as appropriate
		
		// Need to look at javafx.stages[n].title to set titles etc.
		//stages = Resource.slot("javafx", Resource.slot("title", Resource.toJava(Stage::setTitle, String.class)));
		//title = ((Resource)state::updateStageTitle).slot("title").slot("javafx");
		return resources;
	}
	public JavaFXResource() {
		super(makeResources());
	}
	
}
