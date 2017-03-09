package net.fortytwo.smsn.brain.io.vcs;

import org.junit.Test;

import java.io.File;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class VCSFormatTest {
    @Test
    public void atomFilesAccepted() throws Exception {
        assertTrue(VCSFormat.isAtomFile(new File("UuYPT6ck7jBL6abZ")));
        assertTrue(VCSFormat.isAtomFile(new File("0KOr3InLzs3sgAEK")));

        assertTrue(VCSFormat.isAtomFile(new File("UuYPT")));
        File dir = new File("/tmp");
        assertTrue(VCSFormat.isAtomFile(new File(dir, "UuYPT")));
    }

    @Test
    public void nonAtomFilesRejected() throws Exception {
        assertFalse(VCSFormat.isAtomFile(new File("1234")));
        assertFalse(VCSFormat.isAtomFile(new File("?1234567")));
        assertFalse(VCSFormat.isAtomFile(new File("12345678 9")));
        assertFalse(VCSFormat.isAtomFile(new File("a1234567.txt")));
    }

    @Test
    public void oldSchoolAtomFilesRejected() {
        assertFalse(VCSFormat.isAtomFile(new File("au-lH78_")));
        File dir = new File("/tmp");
        assertFalse(VCSFormat.isAtomFile(new File(dir, "a1_234567")));
    }
}
