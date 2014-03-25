package net.fortytwo.extendo.monitron.demos;

import net.fortytwo.extendo.monitron.EventHandler;
import net.fortytwo.extendo.monitron.MonitronService;
import net.fortytwo.extendo.monitron.events.Event;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;
import org.openrdf.model.Statement;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.RDFWriter;
import org.openrdf.rio.Rio;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class RdfizerDemo {
    public static void main(final String[] args) throws ParseException {
        Options options = new Options();

        Option fileOpt = new Option("f", "file", true, "a file from which to load sensor data");
        fileOpt.setRequired(false);
        options.addOption(fileOpt);

        CommandLine cmd = new PosixParser().parse(options, args);
        String fileName = cmd.getOptionValue("file");

        EventHandler handler = new EventHandler() {
            public void handleEvent(Event e) throws EventHandlingException {
                System.out.println("\nreceived dataset:\t\n");
                RDFWriter w = Rio.createWriter(RDFFormat.NQUADS, System.out);
                try {
                    w.startRDF();
                    for (Statement s : e.getDataset().getStatements()) {
                        w.handleStatement(s);
                    }
                    w.endRDF();
                } catch (RDFHandlerException e1) {
                    throw new EventHandler.EventHandlingException(e1);
                }
            }
        };

        try {
            InputStream input = null == fileName ? System.in : new FileInputStream(new File(fileName));
            MonitronService s = new MonitronService(input, handler);
            s.run();
        } catch (Throwable t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }
}
