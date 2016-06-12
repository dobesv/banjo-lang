package banjo.expr.source;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.WeakHashMap;

import banjo.expr.BadExpr;
import banjo.expr.util.FileRange;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;

public class SourceExprFromFile implements SourceExpr {
    public final Path path;
    public SourceExpr memo;
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
    public String toSource() {
        return load().toSource();
    }

    @Override
    public String toSource(Precedence prec) {
        return load().toSource(prec);
    }

    @Override
    public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
        return load().acceptVisitor(visitor);
    }

    @Override
    public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
        return load().acceptVisitor(visitor);
    }

    public synchronized SourceExpr load() {
        FileTime currentFileTime;
        try {
            currentFileTime = Files.getLastModifiedTime(path);
        } catch(IOException e) {
            currentFileTime = null;
        }
        if(this.memo == null || !Objects.equals(currentFileTime, this.memoFileTime)) {
            this.memoFileTime = currentFileTime;
            try {
                this.memo = new SourceExprFactory(this.path).parse();
            } catch(IOException ioe) {
                this.memo = new BadSourceExpr(new SourceFileRange(this.path, FileRange.EMPTY), "Source file cannot be read: " + ioe.toString());
            }
        }
        return this.memo;
    }

    public SourceExprFromFile(Path path) {
        super();
        this.path = path;
    }

    public static final Map<Path, SourceExprFromFile> internedValues = Collections.synchronizedMap(new WeakHashMap<Path, SourceExprFromFile>());

    /**
     * Get a CoreExprFromFile instance that will be shared within the same VM,
     * so that if the same file is loaded twice it won't have to load
     * 
     * @param path
     * @return
     */
    public static SourceExprFromFile forPath(Path path) {
        SourceExprFromFile cached = internedValues.get(path);
        if(cached == null) {
            cached = new SourceExprFromFile(path);
            internedValues.put(path, cached);
        }
        return cached;
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
    public String toFullyParenthesizedSource() {
        return load().toFullyParenthesizedSource();
    }

    @Override
    public void toFullyParenthesizedSource(StringBuffer sb) {
        load().toFullyParenthesizedSource(sb);
    }

    @Override
    public List<BadExpr> getProblems() {
        return load().getProblems();
    }

}
