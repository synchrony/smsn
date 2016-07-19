package net.fortytwo.smsn.server;

import org.openrdf.rio.RDFFormat;

import java.util.LinkedList;
import java.util.List;
import java.util.ServiceLoader;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
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
