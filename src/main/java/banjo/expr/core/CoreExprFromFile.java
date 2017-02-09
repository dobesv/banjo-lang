package banjo.expr.core;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.WeakHashMap;

import banjo.expr.source.SourceExprFromFile;
import fj.P;
import fj.P2;

/**
 * CoreExpr that waits to load itself from a file path until it is actually
 * inspected.
 */
public class CoreExprFromFile extends LazyCoreExpr implements CoreExpr {

    public final Path path;
    public CoreExpr memo;

    @Override
    public CoreExpr calculate() {
        return CoreExpr.fromSourceExpr(SourceExprFromFile.forPath(this.path));
    }

    public CoreExprFromFile(Path path) {
        this.path = path;
    }

    public static final Map<Path, P2<FileTime, CoreExpr>> cache = Collections
            .synchronizedMap(new WeakHashMap<Path, P2<FileTime, CoreExpr>>());

    /**
     * Get a CoreExprFromFile instance that will be shared within the same VM,
     * so that if the same file is loaded twice it won't have to load
     * 
     * @param path
     * @return
     */
    public static CoreExpr forPath(Path path) {
        synchronized (cache) {
            P2<FileTime, CoreExpr> cached = cache.get(path);

            FileTime currentFileTime;
            try {
                currentFileTime = Files.getLastModifiedTime(path);
            } catch (IOException ioe) {
                currentFileTime = null;
            }
            if (cached != null && Objects.equals(currentFileTime, cached._1())) {
                return cached._2();
            }

            CoreExprFromFile expr = new CoreExprFromFile(path);
            cache.put(path, P.p(currentFileTime, expr));
            return expr;
        }
    }
}
