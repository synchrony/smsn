package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.entities.Topic;
import net.fortytwo.smsn.config.DataSource;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

public class VCSFormat extends Format {

    public static Map<String, File> getDirsBySource() {
        Map<String, File> dirs = new HashMap<>();
        for (DataSource dataSource : SemanticSynchrony.getConfiguration().getSources()) {
            dirs.put(dataSource.getName(), new File(dataSource.getLocation()));
        }
        return dirs;
    }

    private static final Pattern SMSN_FILENAME_PATTERN = Pattern.compile("[a-zA-Z0-9]{5,}.smsn");

    private static final VCSFormat instance = new VCSFormat();

    private VCSFormat() {
        super("VCS", Type.Complex);
    }

    public static VCSFormat getInstance() {
        return instance;
    }

    public static boolean isDataFile(final File file) {
        return SMSN_FILENAME_PATTERN.matcher(file.getName()).matches();
    }

    public static String fileNameForTopic(final Topic topic) {
        return topic.getId() + ".smsn";
    }
}
