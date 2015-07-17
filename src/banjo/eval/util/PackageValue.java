package banjo.eval.util;

import java.util.HashMap;

import banjo.eval.SlotNotFound;
import banjo.value.BaseInertValue;
import banjo.value.Value;

/**
 * A Value instance that provides dynamic access to the contents of a package.  This
 * provides slots for each subpackage and class in the package, which are looked up
 * on demand.
 */
public class PackageValue extends BaseInertValue implements Value {
	public final String name;

	public PackageValue(Package pkg) {
	    super();
	    this.name = pkg.getName();
    }

	public PackageValue(String name) {
		this.name = name;
    }

	@Override
	public Value slot(Value self, String slotName, Value fallbackValue) {
		if("label".equals(slotName))
			return Value.fromJava(this.name);
		String childName = this.name+"."+slotName;
		if(Character.isLowerCase(childName.charAt(0))) {
			// Possible subpackage
			Package subpackage = Package.getPackage(childName);
			if(subpackage != null)
				return PackageValue.forName(childName);
		}
		try {
	        return Value.fromClass(Class.forName(childName));
        } catch (ClassNotFoundException | LinkageError e) {
        	if(fallbackValue != null)
        		return fallbackValue;
        	return new SlotNotFound(slotName, self, e);
        }
	}

	public String getName() {
	    return name;
    }

	@Override
	public String toString() {
	    return name;
	}

	public static final HashMap<String, PackageValue> cache = new HashMap<>();
	public static PackageValue forName(String name) {
		PackageValue cached = cache.get(name);
		if(cached == null) {
			cached = new PackageValue(name);
			cache.put(name, cached);
		}
		return cached;
	}

}
