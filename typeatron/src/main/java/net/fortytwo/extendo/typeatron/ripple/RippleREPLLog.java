package net.fortytwo.extendo.typeatron.ripple;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStream;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class RippleREPLLog {
    private final OutputStream out;

    public RippleREPLLog(final String filePath) throws FileNotFoundException {
        boolean append = true;
        out = new FileOutputStream(new File(filePath), append);
    }

    public void logLine(final String line) {

    }
}
