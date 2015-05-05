package banjo.eval.util;

import java.util.function.Supplier;

import fj.data.List;
import banjo.eval.SlotNotFound;
import banjo.eval.Value;

/**
 * A Value instance that provides dynamic access to the contents of a package.  This
 * provides slots for each subpackage and class in the package, which are looked up
 * on demand.
 */
public class PackageValue extends Value {
	public final String name;

	public PackageValue(Package pkg) {
	    super();
	    this.name = pkg.getName();
    }

	public PackageValue(String name) {
		this.name = name;
    }

	@Override
	public Object slot(Object self, String slotName, Supplier<Object> fallbackValue) {
		if("string representation".equals(slotName))
			return this.name;
		String childName = this.name+"."+slotName;
		if(Character.isLowerCase(childName.charAt(0))) {
			// Possible subpackage
			Package subpackage = Package.getPackage(childName);
			if(subpackage != null)
				return new PackageValue(subpackage);
		}
		try {
	        return Class.forName(childName);
        } catch (ClassNotFoundException | LinkageError e) {
        	if(fallbackValue != null)
        		return fallbackValue.get();
        	return new SlotNotFound(slotName, self, e);
        }
	}

	@Override
	public Object callMethod(String slotName, Object targetObject,
	        Supplier<Object> fallback, List<Object> args) {
		String childName = this.name+"."+slotName;
		try {
	        return JavaRuntimeSupport.call(Class.forName(childName), args);
        } catch (ClassNotFoundException | LinkageError e) {
        	if(fallback != null)
        		return fallback.get();
        	return new SlotNotFound(slotName, targetObject, e);
        }
	}

	public String getName() {
	    return name;
    }

	@Override
	public String toString() {
	    return name;
	}

	public static PackageValue forName(String name) {
		return new PackageValue(name);
	}
}
