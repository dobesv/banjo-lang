package banjo.expr.core;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.WeakHashMap;

import banjo.expr.source.Precedence;
import banjo.expr.source.SourceExprFromFile;
import banjo.expr.util.SourceFileRange;
import fj.data.Set;

/**
 * CoreExpr that waits to load itself from a file path until it is actually
 * inspected.
 */
public class CoreExprFromFile implements CoreExpr {

    public final Path path;
    public CoreExpr memo;
    public FileTime memoFileTime;

    @Override
    public void toSource(StringBuffer sb) {
        load().toSource(sb);
    }

    @Override
    public void toSource(StringBuffer sb, Precedence outerPrec) {
        load().toSource(sb, outerPrec);
    }

    @Override
    public String toSource(Precedence prec) {
        return load().toSource(prec);
    }

    @Override
    public String toSource() {
        return load().toSource();
    }

    @Override
    public Precedence getPrecedence() {
        return load().getPrecedence();
    }

    @Override
    public Set<SourceFileRange> getRanges() {
        return load().getRanges();
    }

    @Override
    public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
        return load().acceptVisitor(visitor);
    }

    @Override
    public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
        return load().acceptVisitor(visitor);
    }

    public synchronized CoreExpr load() {
        // Try to get the current file time; if there's an error, use
        // null for the file time.
        FileTime currentFileTime;
        try {
            currentFileTime = Files.getLastModifiedTime(path);
        } catch(IOException e) {
            currentFileTime = null;
        }
        if(this.memo == null || !Objects.equals(currentFileTime, this.memoFileTime)) {
            this.memoFileTime = currentFileTime;
            this.memo = CoreExpr.fromSourceExpr(SourceExprFromFile.forPath(this.path));
        }
        return this.memo;
    }

    public CoreExprFromFile(Path path) {
        this.path = path;
    }

    public static final Map<Path, CoreExprFromFile> internedValues = Collections.synchronizedMap(new WeakHashMap<Path, CoreExprFromFile>());

    /**
     * Get a CoreExprFromFile instance that will be shared within the same VM,
     * so that if the same file is loaded twice it won't have to load
     * 
     * @param path
     * @return
     */
    public static CoreExprFromFile forPath(Path path) {
        CoreExprFromFile cached = internedValues.get(path);
        if(cached == null) {
            cached = new CoreExprFromFile(path);
            internedValues.put(path, cached);
        }
        return cached;
    }
}
