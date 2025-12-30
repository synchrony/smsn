package net.fortytwo.smsn.brain.io.rdf;

import org.eclipse.rdf4j.rio.RDFFormat;

import java.util.LinkedList;
import java.util.List;
import java.util.ServiceLoader;

public class RDFFormatLoader extends ClassLoader {
    public RDFFormatLoader() {
        super(RDFFormatLoader.class.getClassLoader());
    }

    public List<RDFFormat> loadAll() {
        List<RDFFormat> all = new LinkedList<>();

        ServiceLoader<RDFFormat> loader = ServiceLoader.load(RDFFormat.class);
        for (RDFFormat instance : loader) {
            all.add(instance);
        }

        return all;
    }
}
