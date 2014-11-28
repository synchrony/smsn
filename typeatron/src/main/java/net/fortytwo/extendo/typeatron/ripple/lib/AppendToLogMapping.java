package net.fortytwo.extendo.typeatron.ripple.lib;

import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.brain.Filter;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.PrimitiveStackMapping;
import net.fortytwo.ripple.model.RippleList;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class AppendToLogMapping extends PrimitiveStackMapping {

    // TODO: this filter should be updatable
    private final Filter filter;

    private final File logFile;

    private OutputStream keyLog;

    public AppendToLogMapping(Filter filter, File logFile) {
        this.filter = filter;
        this.logFile = logFile;
    }

    public String[] getIdentifiers() {
        return new String[]{
                ExtendoLibrary.NS_2014_12 + "append-to-log"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{
                new Parameter("value", "the value of the atom to append", true),
        };
    }

    public String getComment() {
        return "appends a string as the @value of a new atom in a log";
    }

    public void apply(RippleList stack,
                      Sink<RippleList> solutions,
                      ModelConnection mc) throws RippleException {
        String value = mc.toString(stack.getFirst());
        stack = stack.getRest();

        append(value);

        solutions.put(stack);
    }

    private void append(final String value) throws RippleException {
        try {
            if (null == keyLog) {
                keyLog = new FileOutputStream(logFile, true);
            }

            StringBuilder line = new StringBuilder();
            line.append(Extendo.createRandomKey());
            line.append('\t').append(System.currentTimeMillis());
            line.append('\t').append(filter.getDefaultSharability());
            line.append('\t').append(filter.getDefaultWeight());
            line.append('\t').append(value);
            line.append('\n');

            keyLog.write(line.toString().getBytes());
            keyLog.flush();
        } catch (IOException e) {
            throw new RippleException(e);
        }
    }
}
