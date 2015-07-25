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
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.Iterator;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import banjo.expr.core.BadCoreExpr;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.CoreExprFactory;
import banjo.expr.core.Extend;
import banjo.expr.core.ObjectLiteral;
import banjo.expr.core.Slot;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.SourceExprFactory;
import banjo.expr.token.Identifier;
import banjo.expr.token.StringLiteral;
import banjo.expr.util.FilePos;
import banjo.expr.util.FileRange;
import banjo.expr.util.ParserReader;
import banjo.expr.util.SourceFileRange;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import fj.data.TreeMap;

public class ProjectLoader {
	private static final String LIB_PATH_SYS_PROPERTY = "banjo.path";
	private static final String LIB_PATH_ENV_NAME = "BANJO_PATH";
	public static final List<P2<Identifier, CoreExpr>> EMPTY_BINDINGS = List.nil();

	/**
	 * Load a single binding
	 * @param path
	 * @return
	 */
	public List<P2<Identifier, CoreExpr>> loadBinding(Path path) {
		String[] baseExt = path.getFileName().toString().split("\\.", 2);
		String base = baseExt[0];
		String ext = Files.isRegularFile(path) ? baseExt.length == 2 ? baseExt[1] : "txt" : "";
		Identifier key = new Identifier(base);
		CoreExpr value;
		if(Files.isDirectory(path)) {
			value = loadFolder(path, key);
		} else if(Files.isRegularFile(path)) {
			// System.out.println("Trying to load "+path+" ext = "+ext);
			if(base.isEmpty())
				return EMPTY_BINDINGS;
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
	 * Load the contents of a folder, returning an ObjectLiteral with the AST representation of the folder and its contents.
	 *
	 * @param path Filesystem location of the folder
	 * @param key Name to use for the self-binding of slots in the folder
	 * @return
	 */
	public ObjectLiteral loadFolder(Path path, Identifier key) {
		final List<SourceFileRange> ranges = List.single(new SourceFileRange(path, FileRange.EMPTY));
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
	    CoreExpr value;
	    try {
	    	final SourceExprFactory parser = new SourceExprFactory(path);
	    	final int size = (int)Files.size(path);
	    	if(size == 0) {
	    		value = new BadCoreExpr(new SourceFileRange(path, FileRange.EMPTY), "Empty file '"+path+"'");
	    	} else {
	    		final SourceExpr parsed = parser.parse(new ParserReader(openFile(path), size));
	    		value = new CoreExprFactory().desugar(parsed).getValue();
	    	}
	    } catch (IOException e) {
	    	value = new BadCoreExpr(new SourceFileRange(path, FileRange.EMPTY), "Error reading file '"+path+"': "+e);
	    }
	    return value;
    }

	protected Reader openFile(Path path) throws FileNotFoundException {
	    return new InputStreamReader(new FileInputStream(path.toFile()), Charset.forName("utf8"));
    }

	public CoreExpr loadText(Path path) {
	    CoreExpr value;
	    try {
	    	String text = readFileAsUtf8String(path);
	    	LineNumberReader tmp = new LineNumberReader(new StringReader(text));
	    	tmp.skip(text.length());
	    	final SourceFileRange range = new SourceFileRange(path, new FileRange(FilePos.START, new FilePos(text.length(), tmp.getLineNumber()+1, 1)));
	    	value = new StringLiteral(range, 0, text);
	    } catch (IOException e) {
	    	value = new BadCoreExpr(new SourceFileRange(path, FileRange.EMPTY), "Error reading file '"+path+"': "+e);
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
	    		.map(prevValue -> (CoreExpr) new Extend((CoreExpr)prevValue, binding._2()))
	    		.orSome((CoreExpr)binding._2()));
    }

	public List<P2<Identifier, CoreExpr>> loadBindings(final Path root) {
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
		return loadImportedBindings(path, false);
	}

	public List<P2<Identifier, CoreExpr>> loadClassPathBindings() {
		return loadImportedBindings(System.getProperty("java.class.path", ""), true);
	}
	
	protected String getBanjoPathFromEnvironment() {
	    String path = System.getProperty(LIB_PATH_SYS_PROPERTY);
		if(path == null || path.isEmpty()) path = System.getenv(LIB_PATH_ENV_NAME);
	    return path;
    }

	public static Stream<Path> maybeReadZipFile(Path p) {
		if(Files.isDirectory(p)) {
			return Stream.of(p);
		} else if(Files.isRegularFile(p)) {
			try {
				FileSystem fs = FileSystems.newFileSystem(p, Thread.currentThread().getContextClassLoader());
				return StreamSupport.stream(fs.getRootDirectories().spliterator(), true);
			} catch (IOException e) {
				return Stream.empty();
			}
		} else {
			return Stream.empty();
		}
	}
	public List<P2<Identifier, CoreExpr>> loadImportedBindings(String searchPath, boolean requireDotBanjoFile) {
		if(searchPath == null || searchPath.isEmpty())
			return EMPTY_BINDINGS;
		
		Stream<Path> pathStream = Stream
				.of(searchPath.split(File.pathSeparator))
				.filter(s -> !s.isEmpty())
				.map(Paths::get)
				.flatMap(ProjectLoader::maybeReadZipFile);
		if(requireDotBanjoFile)
			pathStream = pathStream.filter(p -> Files.exists(p.resolve(".banjo")));
		Stream<List<P2<Identifier, CoreExpr>>> bindingsStream = pathStream.map(this::loadBindings);
		return mergeBindings(bindingsStream.reduce(EMPTY_BINDINGS, (a,b) -> a.append(b)));
	}

	protected List<P2<Identifier, CoreExpr>> loadLocalBindings(final Path path) {
		if(path == null)
			return EMPTY_BINDINGS;
		Path tryPath = path;
	    while(tryPath != null) {
			if(Files.exists(tryPath.resolve(".banjo")) || Files.exists(tryPath.resolve(".project")))
				return loadBindings(tryPath);

			tryPath = tryPath.getParent();
		}
		return loadBindings(path);
    }

	public List<P2<Identifier, CoreExpr>> loadLocalAndLibraryBindings(Path sourceFilePath) {
		return loadBanjoPath().append(loadLocalBindings(sourceFilePath));
	}
}
