package banjo.eval;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;



import banjo.desugar.SourceExprDesugarer;
import banjo.dom.core.BadCoreExpr;
import banjo.dom.core.CoreExpr;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.StringLiteral;
import banjo.parser.SourceCodeParser;
import banjo.parser.util.FilePos;
import banjo.parser.util.FileRange;
import banjo.parser.util.ParserReader;
import banjo.parser.util.SourceFileRange;
import fj.data.TreeMap;

public class ProjectLoader {
	public static final TreeMap<Key, CoreExpr> EMPTY_BINDINGS = TreeMap.empty(Key.ORD);

	/**
	 * Load a single binding
	 * @param path
	 * @return
	 */
	public static TreeMap<Key, CoreExpr> loadBinding(Path path) {
		if(!Files.isRegularFile(path))
			return EMPTY_BINDINGS;
		String[] baseExt = path.getFileName().toString().split("\\.", 2);
		String base = baseExt[0];
		String ext = baseExt.length == 2 ? baseExt[1] : Files.isRegularFile(path) ? "txt" : "";
		Key key = new Identifier(base);
		CoreExpr value;
		// System.out.println("Trying to load "+path+" ext = "+ext);
		if(base.isEmpty())
			return EMPTY_BINDINGS;
		final String filename = path.getFileName().toString();
		if("txt".equals(ext)) {
			try {
				String text = new String(Files.readAllBytes(Paths.get("file")), StandardCharsets.UTF_8);
				LineNumberReader tmp = new LineNumberReader(new StringReader(text));
				tmp.skip(text.length());
				final SourceFileRange range = new SourceFileRange(filename, new FileRange(FilePos.START, new FilePos(text.length(), tmp.getLineNumber()+1, 1)));
				value = new StringLiteral(range, text);
			} catch (IOException e) {
				value = new BadCoreExpr(new SourceFileRange(filename, FileRange.EMPTY), "Error reading file '"+path+"': "+e);
			}
		} else if("banjo".equals(ext)) {
			try {
				final SourceCodeParser parser = new SourceCodeParser(filename);
				final int size = (int)Files.size(path);
				if(size == 0) {
					value = new BadCoreExpr(new SourceFileRange(filename, FileRange.EMPTY), "Empty file '"+path+"'");
				} else {
					final SourceExpr parsed = parser.parse(new ParserReader(new FileReader(path.toFile()), filename, size));
					value = new SourceExprDesugarer().desugar(parsed).getValue();
				}
			} catch (IOException e) {
				value = new BadCoreExpr(new SourceFileRange(filename, FileRange.EMPTY), "Error reading file '"+path+"': "+e);
			}
		} else {
			// Skip for now
			return EMPTY_BINDINGS;
		}
		return EMPTY_BINDINGS.set(key, value);
	}

	public static TreeMap<Key, CoreExpr> loadBindings(String rootPath) {
		try {
			return Files.list(Paths.get(rootPath))
					.map(p -> loadBinding(p))
					.reduce(EMPTY_BINDINGS, (b1, b2) -> b2.union(b1));
		} catch (IOException e) {
			return EMPTY_BINDINGS;
		}
	}

	public static TreeMap<Key, CoreExpr> loadBanjoPath() {
		String path = System.getProperty("banjo.path");
		if(path == null) path = System.getenv("BANJO_PATH");
		if(path == null) return EMPTY_BINDINGS;
		return loadImportedBindings(path);
	}

	public static TreeMap<Key, CoreExpr> loadImportedBindings(String searchPath) {
		TreeMap<Key, CoreExpr> bindings = EMPTY_BINDINGS;
		if(searchPath != null) {
			for(String path: searchPath.split(File.pathSeparator)) {
				if(path.isEmpty())
					continue;
				bindings = loadBindings(path).union(bindings);
			}
		}
		return bindings;
	}

	public static TreeMap<Key, CoreExpr> loadLocalBindings(String sourceFilePath) {
		if(sourceFilePath == null)
			return EMPTY_BINDINGS;
		Path p = Paths.get(sourceFilePath);
		while(!(Files.exists(p.resolve(".banjo")) || Files.exists(p.resolve(".project")))) {
			p = p.getParent();
			if(p == null) {
				return EMPTY_BINDINGS;
			}
		}
		return loadBindings(Paths.get(sourceFilePath).toString());
	}

	public static TreeMap<Key, CoreExpr> loadLocalAndLibraryBindings(String sourceFilePath) {
		return loadLocalBindings(sourceFilePath).union(loadBanjoPath());
	}
}
