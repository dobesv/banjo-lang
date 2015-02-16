package banjo.eval;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.Reader;
import java.io.StringReader;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Stream;

import banjo.desugar.SourceExprDesugarer;
import banjo.dom.core.BadCoreExpr;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.Extend;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.Identifier;
import banjo.dom.token.StringLiteral;
import banjo.parser.SourceCodeParser;
import banjo.parser.util.FilePos;
import banjo.parser.util.FileRange;
import banjo.parser.util.ParserReader;
import banjo.parser.util.SourceFileRange;
import fj.P2;
import fj.data.List;
import fj.data.TreeMap;

public class ProjectLoader {
	public static final TreeMap<Identifier, CoreExpr> EMPTY_BINDINGS = TreeMap.empty(Identifier.ORD);

	/**
	 * Load a single binding
	 * @param path
	 * @return
	 */
	public TreeMap<Identifier, CoreExpr> loadBinding(Path path) {
		String[] baseExt = path.getFileName().toString().split("\\.", 2);
		String base = baseExt[0];
		String ext = baseExt.length == 2 ? baseExt[1] : isRegularFile(path) ? "txt" : "";
		Identifier key = new Identifier(base);
		CoreExpr value;
		if(isDirectory(path)) {
			value = loadFolder(path);
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
		return EMPTY_BINDINGS.set(key, value);
	}

	protected boolean isDirectory(Path path) {
	    return Files.isDirectory(path);
    }

	protected boolean isRegularFile(Path path) {
	    return Files.isRegularFile(path);
    }

	public ObjectLiteral loadFolder(Path path) {
		final List<SourceFileRange> ranges = List.single(new SourceFileRange(path.getFileName().toString(), FileRange.EMPTY));
		try {
			return new ObjectLiteral(ranges,
					listFilesInFolder(path)
					.map(p -> loadBinding(p))
					.reduce(EMPTY_BINDINGS, (b1, b2) -> mergeBindings(b1, b2))
					.toStream()
					.toList());


		} catch (IOException e) {
			return new ObjectLiteral(ranges, List.nil());
		}
    }

	public Stream<Path> listFilesInFolder(Path path) throws IOException {
	    return Files.list(path);
    }

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
	    		value = new SourceExprDesugarer().desugar(parsed).getValue();
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
		Reader reader = openFile(path);
		StringBuilder sb = new StringBuilder();
		CharBuffer buf = CharBuffer.allocate(64*1024);
		while(reader.read(buf) > 0) {
			buf.flip();
			sb.append(buf.array(), 0, buf.length());
			buf.clear();
		}
	    return sb.toString();
    }

	public static TreeMap<Identifier, CoreExpr> mergeBindings(TreeMap<Identifier, CoreExpr> a, TreeMap<Identifier, CoreExpr> b) {
		for(P2<Identifier, CoreExpr> bb : b) {
			// TODO Object properties inside the values should probably also be merged, somehow, (i.e. subfolders)
			a = a.set(bb._1(), a.get(bb._1()).map(ab -> (CoreExpr)new Extend(ab, bb._2())).orSome(bb._2()));
		}
		return a;
	}
	public TreeMap<Identifier, CoreExpr> loadBindings(String rootPath) {
		return loadBindings(Paths.get(rootPath));
	}

	protected TreeMap<Identifier, CoreExpr> loadBindings(final Path root) {
	    try {
			return listFilesInFolder(root)
					.map(p -> loadBinding(p))
					.reduce(EMPTY_BINDINGS, (b1, b2) -> mergeBindings(b1, b2));
		} catch (IOException e) {
			return EMPTY_BINDINGS;
		}
    }

	public TreeMap<Identifier, CoreExpr> loadBanjoPath() {
		String path = getBanjoPathFromEnvironment();
		if(path == null) return EMPTY_BINDINGS;
		return loadImportedBindings(path);
	}

	protected String getBanjoPathFromEnvironment() {
	    String path = System.getProperty("banjo.path");
		if(path == null) path = System.getenv("BANJO_PATH");
	    return path;
    }

	public TreeMap<Identifier, CoreExpr> loadImportedBindings(String searchPath) {
		TreeMap<Identifier, CoreExpr> bindings = EMPTY_BINDINGS;
		if(searchPath != null) {
			for(String path: searchPath.split(File.pathSeparator)) {
				if(path.isEmpty())
					continue;
				bindings = mergeBindings(bindings, loadBindings(path));
			}
		}
		return bindings;
	}

	public TreeMap<Identifier, CoreExpr> loadLocalBindings(String sourceFilePath) {
		if(sourceFilePath == null)
			return EMPTY_BINDINGS;
		Path path = Paths.get(sourceFilePath);
		return loadLocalBindings(path.getParent());
	}

	protected TreeMap<Identifier, CoreExpr> loadLocalBindings(final Path path) {
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

	public TreeMap<Identifier, CoreExpr> loadLocalAndLibraryBindings(String sourceFilePath) {
		return loadLocalBindings(sourceFilePath).union(loadBanjoPath());
	}
}
