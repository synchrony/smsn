package net.fortytwo.smsn.brain.io;

import com.google.common.base.Preconditions;

import java.util.HashMap;
import java.util.Map;
import java.util.ServiceLoader;
import java.util.logging.Logger;

public class Format {
    private static final Logger logger = Logger.getLogger(Format.class.getName());

    public enum Type {Internal, FileBased, Complex }

    private static final Map<String, Format> formatsByNameLowercase;
    private static final Map<Format, NoteReader> readersByFormat;
    private static final Map<Format, NoteWriter> writersByFormat;
    private static boolean initialized;

    static {
        formatsByNameLowercase = new HashMap<>();
        readersByFormat = new HashMap<>();
        writersByFormat = new HashMap<>();
    }

    private final String name;
    private final Type type;
    private final String[] fileExtensions;

    public Format(String name, Type type, String... fileExtensions) {
        this.type = type;
        this.name = name;
        this.fileExtensions = fileExtensions;

        if (null == name) throw new IllegalArgumentException();
        if (0 == name.length()) throw new IllegalArgumentException();
        if (null == fileExtensions) throw new IllegalArgumentException();
    }

    public String getName() {
        return name;
    }

    public Type getType() {
        return type;
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

    public static NoteReader getReader(final Format format) {
        checkInitialized();

        NoteReader reader = readersByFormat.get(format);

        if (null == reader) {
            throw new IllegalStateException("no reader for format: " + format);
        }

        return reader;
    }

    public static NoteWriter getWriter(final Format format) {
        if (null == format) throw new IllegalArgumentException();

        checkInitialized();

        NoteWriter writer = writersByFormat.get(format);

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
        ServiceLoader<NoteReader> loader = ServiceLoader.load(NoteReader.class);
        int count = 0;
        for (NoteReader reader : loader) {
            Preconditions.checkNotNull(reader.getFormats());
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
        ServiceLoader<NoteWriter> loader = ServiceLoader.load(NoteWriter.class);
        int count = 0;
        for (NoteWriter writer : loader) {
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
