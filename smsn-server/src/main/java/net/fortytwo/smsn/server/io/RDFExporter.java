package net.fortytwo.smsn.server.io;

import net.fortytwo.smsn.brain.MyOtherBrain;
import net.fortytwo.smsn.server.RDFFormatLoader;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.sail.SailException;

import java.io.IOException;
import java.io.OutputStream;
import java.util.LinkedList;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class RDFExporter extends Exporter {

    @Override
    public List<String> getFormats() {
        List<String> formats = new LinkedList<>();
        for (RDFFormat rdfFormat : new RDFFormatLoader().loadAll()) {
            formats.add(rdfFormat.getName());
        }
        return formats;
    }

    private final RDFFormat format;

    public RDFExporter(final RDFFormat format) {
        this.format = format;
    }

    @Override
    protected void exportInternal(MyOtherBrain sourceBrain, OutputStream destStream) throws IOException {
        try {
            sourceBrain.getKnowledgeBase().exportRDF(destStream, format, requireFilter());
        } catch (SailException | RDFHandlerException e) {
            throw new IOException(e);
        }
        destStream.flush();
        destStream.close();
    }
}
