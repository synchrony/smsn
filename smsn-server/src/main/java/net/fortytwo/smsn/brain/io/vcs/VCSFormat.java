package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.brain.io.Format;

import java.io.File;
import java.util.regex.Pattern;

public class VCSFormat extends Format {

    public static File[] getDirsBySharability(final File parentDir) {
        return new File[]{
                new File(parentDir, "private"),
                new File(parentDir, "personal"),
                new File(parentDir, "public"),
                new File(parentDir, "universal")};
    }

    private static final Pattern ATOM_FILENAME_PATTERN = Pattern.compile("a[a-zA-Z0-9-_]{5,}");

    private static final VCSFormat instance = new VCSFormat();

    private VCSFormat() {
        super("VCS", Type.DirectoryBased);
    }

    public static VCSFormat getInstance() {
        return instance;
    }

    public static boolean isAtomFile(final File file) {
        return ATOM_FILENAME_PATTERN.matcher(file.getName()).matches();
    }
}
