package banjo.eval;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.Reader;
import java.io.StringReader;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.Iterator;
import java.util.stream.Stream;

import banjo.desugar.SourceExprToCoreExpr;
import banjo.dom.core.BadCoreExpr;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.Extend;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.core.Slot;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.Identifier;
import banjo.dom.token.StringLiteral;
import banjo.parser.SourceCodeParser;
import banjo.parser.util.FilePos;
import banjo.parser.util.FileRange;
import banjo.parser.util.ParserReader;
import banjo.parser.util.SourceFileRange;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import fj.data.TreeMap;

public class ProjectLoader {
	public static final List<P2<Identifier, CoreExpr>> EMPTY_BINDINGS = List.nil();

	/**
	 * Load a single binding
	 * @param path
	 * @return
	 */
	public List<P2<Identifier, CoreExpr>> loadBinding(Path path) {
		String[] baseExt = path.getFileName().toString().split("\\.", 2);
		String base = baseExt[0];
		String ext = isRegularFile(path) ? baseExt.length == 2 ? baseExt[1] : "txt" : "";
		Identifier key = new Identifier(base);
		CoreExpr value;
		if(isDirectory(path)) {
			value = loadFolder(path, key);
		} else if(isRegularFile(path)) {
			// System.out.println("Trying to load "+path+" ext = "+ext);
			if(base.isEmpty())
				return EMPTY_BINDINGS;
			final String filename = path.getFileName().toString();
			if("txt".equals(ext)) {
				value = loadText(path);
			} else if("banjo".equals(ext)) {
				value = loadSourceCode(path);
			} else {
				// Skip for now
				return EMPTY_BINDINGS;
			}
		} else return EMPTY_BINDINGS;
		return List.single(P.p(key, value));
	}

	/**
	 * True if a directory exists at the target path.  Follows symlinks.
	 */
	protected boolean isDirectory(Path path) {
	    return Files.isDirectory(path);
    }

	/**
	 * True if the file is a "regular" file, i.e. it exists and is not a folder, device, or symlink to something that is
	 * not a regular file.
	 */
	protected boolean isRegularFile(Path path) {
	    return Files.isRegularFile(path);
    }

	/**
	 * Load the contents of a folder, returning an ObjectLiteral with the AST representation of the folder and its contents.
	 *
	 * @param path Filesystem location of the folder
	 * @param key Name to use for the self-binding of slots in the folder
	 * @return
	 */
	public ObjectLiteral loadFolder(Path path, Identifier key) {
		final List<SourceFileRange> ranges = List.single(new SourceFileRange(path.getFileName().toString(), FileRange.EMPTY));
		try {
			Option<Identifier> selfBinding = Option.some(key);
			return new ObjectLiteral(ranges, mergeBindings(listFilesInFolder(path)
					.map(p -> loadBinding(p))
					.reduce(EMPTY_BINDINGS, (b1, b2) -> b1.append(b2))
			).map(p -> new Slot(p._1(), selfBinding, p._2())));
		} catch (IOException e) {
			return new ObjectLiteral(ranges, List.nil());
		}
    }

	/**
	 * List files in a folder.
	 *
	 * The files are unsorted.
	 *
	 * A subclass (i.e. as in eclipse) might override this.
	 */
	public Stream<Path> listFilesInFolder(Path path) throws IOException {
	    return Files.list(path);
    }

	/**
	 * Return a sorted list of files in the given folder.  Uses the listFilesInFolder() method to get
	 * the list of files, which allows the file listing method to be overridden more easily.
	 *
	 * This sorts folders to the top, then alphabetically.
	 */
	public Stream<Path> sortedFilesInFolder(Path path) throws IOException {
		return listFilesInFolder(path).sorted(new Comparator<Path>() {
	    	@Override
	    	public int compare(Path o1, Path o2) {
	    		int cmp = -Boolean.compare(Files.isDirectory(o1), Files.isDirectory(o2));
	    		if(cmp != 0) return cmp;

	    		for(Iterator<Path> i1 = o1.iterator(), i2 = o2.iterator(); i1.hasNext() || i2.hasNext(); ) {
	    			if(!i1.hasNext()) return 1; // i2 has more elements
	    			if(!i2.hasNext()) return -1; // i1 has more elements
	    			final String s1 = i1.next().toString();
					final String s2 = i2.next().toString();
					cmp = s1.compareToIgnoreCase(s2);
	    			if(cmp != 0) return cmp;
	    			cmp = s1.compareTo(s2);
	    			if(cmp != 0) return cmp;
	    		}
	    		return cmp;
	    	}
		});
	}

	/**
	 * Load an expression tree from a file.
	 *
	 * @param path File path to load
	 * @return The expression tree
	 */
	public CoreExpr loadSourceCode(Path path) {
    	final String filePath = path.toString();
	    CoreExpr value;
	    try {
	    	final SourceCodeParser parser = new SourceCodeParser(filePath);
	    	final int size = (int)fileSize(path);
	    	if(size == 0) {
	    		value = new BadCoreExpr(new SourceFileRange(filePath, FileRange.EMPTY), "Empty file '"+filePath+"'");
	    	} else {
	    		final SourceExpr parsed = parser.parse(new ParserReader(openFile(path), size));
	    		value = new SourceExprToCoreExpr().desugar(parsed).getValue();
	    	}
	    } catch (IOException e) {
	    	value = new BadCoreExpr(new SourceFileRange(filePath, FileRange.EMPTY), "Error reading file '"+filePath+"': "+e);
	    }
	    return value;
    }

	protected long fileSize(Path path) throws IOException {
	    return Files.size(path);
    }

	protected Reader openFile(Path path) throws FileNotFoundException {
	    return new InputStreamReader(new FileInputStream(path.toFile()), Charset.forName("utf8"));
    }

	public CoreExpr loadText(Path path) {
    	String filePath = path.toString();
	    CoreExpr value;
	    try {
	    	String text = readFileAsUtf8String(path);
	    	LineNumberReader tmp = new LineNumberReader(new StringReader(text));
	    	tmp.skip(text.length());
	    	final SourceFileRange range = new SourceFileRange(filePath, new FileRange(FilePos.START, new FilePos(text.length(), tmp.getLineNumber()+1, 1)));
	    	value = new StringLiteral(range, text);
	    } catch (IOException e) {
	    	value = new BadCoreExpr(new SourceFileRange(filePath, FileRange.EMPTY), "Error reading file '"+path+"': "+e);
	    }
	    return value;
    }

	protected String readFileAsUtf8String(Path path)
            throws IOException {
		try(Reader reader = openFile(path)) {
			StringBuilder sb = new StringBuilder();
			CharBuffer buf = CharBuffer.allocate(64*1024);
			while(reader.read(buf) > 0) {
				buf.flip();
				sb.append(buf.array(), 0, buf.length());
				buf.clear();
			}
		    return sb.toString();
		}
    }

	/**
	 * Merge same-named bindings in the list into a single binding for each name.
	 *
	 * The result does not maintain the original sort order of the list.
	 */
	public static List<P2<Identifier, CoreExpr>> mergeBindings(
            List<P2<Identifier, CoreExpr>> bindings) {
		final TreeMap<Identifier, CoreExpr> newBindingMap = bindings.foldLeft(ProjectLoader::mergeBinding, TreeMap.empty(Identifier.ORD));
		return newBindingMap.toStream().toList();
    }

	protected static TreeMap<Identifier, CoreExpr> mergeBinding(
            TreeMap<Identifier, CoreExpr> bindingMap, P2<Identifier, CoreExpr> binding) {
	    return bindingMap.set(binding._1(),
	    		bindingMap.get(binding._1())
	    		.map(nextValue -> (CoreExpr) new Extend(binding._2(), (CoreExpr)nextValue))
	    		.orSome((CoreExpr)binding._2()));
    }

	public List<P2<Identifier, CoreExpr>> loadBindings(String rootPath) {
		return loadBindings(Paths.get(rootPath));
	}

	protected List<P2<Identifier, CoreExpr>> loadBindings(final Path root) {
	    try {
			return mergeBindings(listFilesInFolder(root)
					.map(p -> loadBinding(p))
					.reduce(EMPTY_BINDINGS, (b1, b2) -> b1.append(b2)));
		} catch (IOException e) {
			return EMPTY_BINDINGS;
		}
    }

	public List<P2<Identifier, CoreExpr>> loadBanjoPath() {
		String path = getBanjoPathFromEnvironment();
		if(path == null) return EMPTY_BINDINGS;
		return loadImportedBindings(path);
	}

	protected String getBanjoPathFromEnvironment() {
	    String path = System.getProperty("banjo.path");
		if(path == null) path = System.getenv("BANJO_PATH");
	    return path;
    }

	public List<P2<Identifier, CoreExpr>> loadImportedBindings(String searchPath) {
		List<P2<Identifier, CoreExpr>> bindings = EMPTY_BINDINGS;
		if(searchPath != null) {
			for(String path: searchPath.split(File.pathSeparator)) {
				if(path.isEmpty())
					continue;
				bindings = bindings.append(loadBindings(path));
			}
		}
		return mergeBindings(bindings);
	}

	public List<P2<Identifier, CoreExpr>> loadLocalBindings(String sourceFilePath) {
		if(sourceFilePath == null)
			return EMPTY_BINDINGS;
		Path path = Paths.get(sourceFilePath);
		return loadLocalBindings(path.getParent());
	}

	protected List<P2<Identifier, CoreExpr>> loadLocalBindings(final Path path) {
		if(path == null)
			return EMPTY_BINDINGS;
		Path tryPath = path;
	    while(tryPath != null) {
			if(fileExists(tryPath.resolve(".banjo")) || fileExists(tryPath.resolve(".project")))
				return loadBindings(tryPath);

			tryPath = tryPath.getParent();
		}
		return loadBindings(path);
    }

	protected boolean fileExists(Path p) {
	    return Files.exists(p);
    }

	public List<P2<Identifier, CoreExpr>> loadLocalAndLibraryBindings(String sourceFilePath) {
		return loadBanjoPath().append(loadLocalBindings(sourceFilePath));
	}
}
