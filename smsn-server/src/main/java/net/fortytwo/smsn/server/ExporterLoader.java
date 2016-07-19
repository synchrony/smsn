package net.fortytwo.smsn.server;

import net.fortytwo.smsn.server.io.Exporter;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ExporterLoader extends ClassLoader {
    protected static final Logger logger = Logger.getLogger(ExporterLoader.class.getName());

    public ExporterLoader() {
        super(ExporterLoader.class.getClassLoader());
    }

    public Map<String, Exporter> loadAll() {
        Map<String, Exporter> all = new HashMap<>();

        ServiceLoader<Exporter> loader = ServiceLoader.load(Exporter.class);
        for (Exporter instance : loader) {
            if (0 == instance.getFormats().size()) {
                logger.warning("exporter has no formats: " + instance);
            }
            for (String format : instance.getFormats()) {
                all.put(format, instance);
            }
        }

        return all;
    }
}
