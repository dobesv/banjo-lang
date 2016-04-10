package banjo.expr.util;

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;

import fj.data.List;

public class PathUtils {

    /**
     * Decode possibly a URL-encoded filename.
     * <p>
     * This doesn't use the java provided URLDecoder because it follows slightly
     * different rules, such as not processing '+'.
     * 
     * @param s
     *            Path component to URL-decode
     * @return A URL-decoded filename
     * @throws UnsupportedEncodingException
     */
    public static String decodeFilename(String s)
        throws UnsupportedEncodingException {
    
        boolean needToChange = false;
        int numChars = s.length();
        StringBuffer sb = new StringBuffer(numChars > 500 ? numChars / 2 : numChars);
        int i = 0;
    
        char c;
        byte[] bytes = null;
        while(i < numChars) {
            c = s.charAt(i);
            switch(c) {
            case '%':
                /*
                 * Starting with this instance of %, process all consecutive
                 * substrings of the form %xy. Each substring %xy will yield a
                 * byte. Convert all consecutive bytes obtained this way to
                 * whatever character(s) they represent in the provided
                 * encoding.
                 */
    
                try {
    
                    // (numChars-i)/3 is an upper bound for the number
                    // of remaining bytes
                    if(bytes == null)
                        bytes = new byte[(numChars - i) / 3];
                    int pos = 0;
    
                    while(((i + 2) < numChars) &&
                        (c == '%')) {
                        int v = Integer.parseInt(s.substring(i + 1, i + 3), 16);
                        if(v < 0)
                            throw new IllegalArgumentException("URLDecoder: Illegal hex characters in escape (%) pattern - negative value");
                        bytes[pos++] = (byte) v;
                        i += 3;
                        if(i < numChars)
                            c = s.charAt(i);
                    }
    
                    // A trailing, incomplete byte encoding such as
                    // "%x" will cause an exception to be thrown
    
                    if((i < numChars) && (c == '%'))
                        throw new IllegalArgumentException(
                            "URLDecoder: Incomplete trailing escape (%) pattern");
    
                    sb.append(new String(bytes, 0, pos, "UTF-8"));
                } catch(NumberFormatException e) {
                    throw new IllegalArgumentException(
                        "URLDecoder: Illegal hex characters in escape (%) pattern - " + e.getMessage());
                }
                needToChange = true;
                break;
            default:
                sb.append(c);
                i++;
                break;
            }
        }
    
        return (needToChange ? sb.toString() : s);
    }

    /**
     * If the given Path instance refers to a zip or jar file, return a Path
     * instance referencing the root folder of the archive rather than the
     * archive file itself.
     * <p>
     * Any other paths are returned unchanged.
     * 
     * @param p
     * @return
     */
    public static Path zipFileToZipPath(Path p) {
        try {
            if(Files.isRegularFile(p) && p.getFileName().toString().matches("\\.(zip|jar)$")) {
                FileSystem fs = FileSystems.newFileSystem(p, Thread.currentThread().getContextClassLoader());
                Path root = fs.getRootDirectories().iterator().next();
                return root;
            }
        } catch(IOException e) {
            // Not a valid ZIP file? Not sure what to do here
        }
        return p;
    }

    /**
     * True if the given string is a valid pathname in the default FileSystem.
     * 
     * @param p
     *            Path to check
     * @return true if the path is valid
     */
    public static boolean isValidPath(String p) {
        try {
            Paths.get(p);
            return true;
        } catch(InvalidPathException e) {
            return false;
        }
    }

    /**
     * Convert a search path string such as from CLASSPATH or java.class.path or
     * banjo.path and return a list of Path instances for that path.
     * <p>
     * If the path entry refers to a Zip file the resulting Path instance will
     * refer to the root folder of the ZIP rather than the Zip file itself.
     * <p>
     * Only paths in the search path that exist and are valid will be included
     * in the result.
     * 
     * @param searchPath
     *            Search path string; each path is separated by
     *            File.pathSeparator (semicolon in windows, colon elsewhere)
     * @return A list of Path instances
     */
    public static List<Path> pathsFromSearchPath(String searchPath) {
        return List.list(searchPath.split(File.pathSeparator))
            .filter(s -> !s.isEmpty())
            .filter(PathUtils::isValidPath)
            .map(Paths::get)
            .filter(Files::exists)
            .map(PathUtils::zipFileToZipPath);
    }


}
