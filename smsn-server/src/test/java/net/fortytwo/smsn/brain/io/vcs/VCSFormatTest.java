package net.fortytwo.smsn.brain.io.vcs;

import org.junit.Test;

import java.io.File;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class VCSFormatTest {
    @Test
    public void atomFilesAccepted() throws Exception {
        assertTrue(VCSFormat.isAtomFile(new File("a1234567")));
        assertTrue(VCSFormat.isAtomFile(new File("a12345")));
        assertTrue(VCSFormat.isAtomFile(new File("au-lH78_")));
        File dir = new File("/tmp");
        assertTrue(VCSFormat.isAtomFile(new File(dir, "a1234567")));
    }

    @Test
    public void nonAtomFilesRejected() throws Exception {
        assertFalse(VCSFormat.isAtomFile(new File("a1234")));
        assertFalse(VCSFormat.isAtomFile(new File("1234567")));
        assertFalse(VCSFormat.isAtomFile(new File("12345678")));
        assertFalse(VCSFormat.isAtomFile(new File("a 1234567")));
        assertFalse(VCSFormat.isAtomFile(new File("b1234567")));
        assertFalse(VCSFormat.isAtomFile(new File("a1234567.txt")));
    }
}
