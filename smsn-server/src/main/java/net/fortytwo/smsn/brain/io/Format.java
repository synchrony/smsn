package net.fortytwo.smsn.brain.io;

import java.util.HashMap;
import java.util.Map;
import java.util.ServiceLoader;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Format {
    private static final Logger logger = Logger.getLogger(Format.class.getName());

    private static final Map<String, Format> formatsByNameLowercase;
    private static final Map<Format, BrainReader> readersByFormat;
    private static final Map<Format, BrainWriter> writersByFormat;
    private static boolean initialized;

    static {
        formatsByNameLowercase = new HashMap<>();
        readersByFormat = new HashMap<>();
        writersByFormat = new HashMap<>();
    }

    private final String name;
    private final String[] fileExtensions;

    public Format(String name, String[] fileExtensions) {
        this.name = name;
        this.fileExtensions = fileExtensions;

        if (null == name) throw new IllegalArgumentException();
        if (0 == name.length()) throw new IllegalArgumentException();
        if (null == fileExtensions) throw new IllegalArgumentException();
    }

    public String getName() {
        return name;
    }

    public String[] getFileExtensions() {
        return fileExtensions;
    }

    @Override
    public String toString() {
        return getName();
    }

    public static Format getFormat(final String formatNameIgnoreCase) {
        checkInitialized();

        Format format = formatsByNameLowercase.get(formatNameIgnoreCase.toLowerCase());

        if (null == format) {
            throw new IllegalArgumentException("unknown format: " + formatNameIgnoreCase);
        }

        return format;
    }

    public static BrainReader getReader(final Format format) {
        checkInitialized();

        BrainReader reader = readersByFormat.get(format);

        if (null == reader) {
            throw new IllegalStateException();
        }

        return reader;
    }

    public static BrainWriter getWriter(final Format format) {
        if (null == format) throw new IllegalArgumentException();

        checkInitialized();

        BrainWriter writer = writersByFormat.get(format);

        if (null == writer) {
            throw new IllegalStateException("no writer for format " + format);
        }

        return writer;
    }

    private synchronized static void checkInitialized() {
        if (initialized) return;

        initializeReaders();
        initializeWriters();

        initialized = true;
    }

    private static void initializeReaders() {
        ServiceLoader<BrainReader> loader = ServiceLoader.load(BrainReader.class);
        int count = 0;
        for (BrainReader reader : loader) {
            if (0 == reader.getFormats().size()) {
                logger.warning("reader has no formats: " + reader);
            }
            for (Format format : reader.getFormats()) {
                addFormat(format, reader);
                readersByFormat.put(format, reader);
            }
            count++;
        }

        logger.info("loaded " + count + " readers");
    }

    private static void initializeWriters() {
        ServiceLoader<BrainWriter> loader = ServiceLoader.load(BrainWriter.class);
        int count = 0;
        for (BrainWriter writer : loader) {
            if (0 == writer.getFormats().size()) {
                logger.warning("writer has no formats: " + writer);
            }
            for (Format format : writer.getFormats()) {
                addFormat(format, writer);
                writersByFormat.put(format, writer);
            }
            count++;
        }
        logger.info("loaded " + count + " writers");
    }

    private static void addFormat(final Format format, final Object owner) {
        if (null == format) {
            throw new IllegalStateException("null format for " + owner);
        }

        formatsByNameLowercase.put(format.getName().toLowerCase(), format);
    }
}
