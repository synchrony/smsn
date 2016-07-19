package net.fortytwo.smsn.server;

import net.fortytwo.smsn.server.io.Importer;

import java.util.HashMap;
import java.util.Map;
import java.util.ServiceLoader;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ImporterLoader extends ClassLoader {
    protected static final Logger logger = Logger.getLogger(ImporterLoader.class.getName());

    public ImporterLoader() {
        super(ImporterLoader.class.getClassLoader());
    }

    public Map<String, Importer> loadAll() {
        Map<String, Importer> all = new HashMap<>();

        ServiceLoader<Importer> loader = ServiceLoader.load(Importer.class);
        for (Importer instance : loader) {
            if (0 == instance.getFormats().size()) {
                logger.warning("importer has no formats: " + instance);
            }
            for (String format : instance.getFormats()) {
                all.put(format, instance);
            }
        }

        return all;
    }
}
