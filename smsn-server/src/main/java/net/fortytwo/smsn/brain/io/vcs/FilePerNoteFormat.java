package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.entities.Topic;
import net.fortytwo.smsn.config.DataSource;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

public class FilePerNoteFormat extends Format {
    private final String extension;
    private final Pattern fileNamePattern;

    public FilePerNoteFormat(String name, String extension) {
        super(name, Type.Complex);
        this.extension = extension;
        this.fileNamePattern = Pattern.compile("[a-zA-Z0-9]{13,}." + extension);
    }

    public static Map<String, File> directoriesBySource() {
        Map<String, File> dirs = new HashMap<>();
        for (DataSource dataSource : SemanticSynchrony.getConfiguration().getSources()) {
            dirs.put(dataSource.getName(), new File(dataSource.getLocation()));
        }
        return dirs;
    }

    public boolean isMatchingFile(final File file) {
        return fileNamePattern.matcher(file.getName()).matches();
    }

    public String fileNameFor(final Topic topic) {
        return topic.getId().value + "." + extension;
    }
}
