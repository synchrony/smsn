package net.fortytwo.smsn.typeatron.ripple;

import net.fortytwo.smsn.typeatron.ripple.ExtendedCharacters.Diacritic;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class ExtendedCharactersTest {

    private ExtendedCharacters chars;

    @Before
    public void setUp() {
        chars = new ExtendedCharacters();
    }

    @Test
    public void graveAccentOnVowels() {
        assertEquals(Character.valueOf('\u00c0'), chars.modify(Diacritic.Grave, 'A'));  // À
        assertEquals(Character.valueOf('\u00e0'), chars.modify(Diacritic.Grave, 'a'));  // à
        assertEquals(Character.valueOf('\u00c8'), chars.modify(Diacritic.Grave, 'E'));  // È
        assertEquals(Character.valueOf('\u00e8'), chars.modify(Diacritic.Grave, 'e'));  // è
        assertEquals(Character.valueOf('\u00cc'), chars.modify(Diacritic.Grave, 'I'));  // Ì
        assertEquals(Character.valueOf('\u00ec'), chars.modify(Diacritic.Grave, 'i'));  // ì
        assertEquals(Character.valueOf('\u00d2'), chars.modify(Diacritic.Grave, 'O'));  // Ò
        assertEquals(Character.valueOf('\u00f2'), chars.modify(Diacritic.Grave, 'o'));  // ò
        assertEquals(Character.valueOf('\u00d9'), chars.modify(Diacritic.Grave, 'U'));  // Ù
        assertEquals(Character.valueOf('\u00f9'), chars.modify(Diacritic.Grave, 'u'));  // ù
    }

    @Test
    public void acuteAccentOnVowels() {
        assertEquals(Character.valueOf('\u00c1'), chars.modify(Diacritic.Acute, 'A'));  // Á
        assertEquals(Character.valueOf('\u00e1'), chars.modify(Diacritic.Acute, 'a'));  // á
        assertEquals(Character.valueOf('\u00c9'), chars.modify(Diacritic.Acute, 'E'));  // É
        assertEquals(Character.valueOf('\u00e9'), chars.modify(Diacritic.Acute, 'e'));  // é
    }

    @Test
    public void circumflexOnVowels() {
        assertEquals(Character.valueOf('\u00c2'), chars.modify(Diacritic.Circumflex, 'A'));  // Â
        assertEquals(Character.valueOf('\u00e2'), chars.modify(Diacritic.Circumflex, 'a'));  // â
        assertEquals(Character.valueOf('\u00ca'), chars.modify(Diacritic.Circumflex, 'E'));  // Ê
        assertEquals(Character.valueOf('\u00ea'), chars.modify(Diacritic.Circumflex, 'e'));  // ê
    }

    @Test
    public void tildeOnATildeOnN() {
        assertEquals(Character.valueOf('\u00c3'), chars.modify(Diacritic.Tilde, 'A'));  // Ã
        assertEquals(Character.valueOf('\u00e3'), chars.modify(Diacritic.Tilde, 'a'));  // ã
        assertEquals(Character.valueOf('\u00d1'), chars.modify(Diacritic.Tilde, 'N'));  // Ñ
        assertEquals(Character.valueOf('\u00f1'), chars.modify(Diacritic.Tilde, 'n'));  // ñ
    }

    @Test
    public void dieresisOnVowels() {
        assertEquals(Character.valueOf('\u00c4'), chars.modify(Diacritic.Dieresis, 'A'));  // Ä
        assertEquals(Character.valueOf('\u00e4'), chars.modify(Diacritic.Dieresis, 'a'));  // ä
        assertEquals(Character.valueOf('\u00d6'), chars.modify(Diacritic.Dieresis, 'O'));  // Ö
        assertEquals(Character.valueOf('\u00f6'), chars.modify(Diacritic.Dieresis, 'o'));  // ö
        assertEquals(Character.valueOf('\u00dc'), chars.modify(Diacritic.Dieresis, 'U'));  // Ü
        assertEquals(Character.valueOf('\u00fc'), chars.modify(Diacritic.Dieresis, 'u'));  // ü
    }

    @Test
    public void overringOnA() {
        assertEquals(Character.valueOf('\u00c5'), chars.modify(Diacritic.Overring, 'A'));  // Å
        assertEquals(Character.valueOf('\u00e5'), chars.modify(Diacritic.Overring, 'a'));  // å
    }

    @Test
    public void slashOnO() {
        assertEquals(Character.valueOf('\u00d8'), chars.modify(Diacritic.Slash, 'O'));  // Ø
        assertEquals(Character.valueOf('\u00f8'), chars.modify(Diacritic.Slash, 'o'));  // ø
    }

    @Test
    public void cedillaOnC() {
        // C cedilla is mapped via Acute diacritic
        assertEquals(Character.valueOf('\u00c7'), chars.modify(Diacritic.Acute, 'C'));  // Ç
        assertEquals(Character.valueOf('\u00e7'), chars.modify(Diacritic.Acute, 'c'));  // ç
    }

    @Test
    public void ethAndThorn() {
        // Eth (Icelandic/Old English)
        assertEquals(Character.valueOf('\u00d0'), chars.modify(Diacritic.Slash, 'D'));  // Ð
        assertEquals(Character.valueOf('\u00f0'), chars.modify(Diacritic.Slash, 'd'));  // ð
        // Thorn (Icelandic/Old English)
        assertEquals(Character.valueOf('\u00de'), chars.modify(Diacritic.Slash, 'T'));  // Þ
        assertEquals(Character.valueOf('\u00fe'), chars.modify(Diacritic.Slash, 't'));  // þ
    }

    @Test
    public void yAcuteAndDieresis() {
        assertEquals(Character.valueOf('\u00dd'), chars.modify(Diacritic.Acute, 'Y'));  // Ý
        assertEquals(Character.valueOf('\u00fd'), chars.modify(Diacritic.Acute, 'y'));  // ý
        assertEquals(Character.valueOf('\u00ff'), chars.modify(Diacritic.Dieresis, 'y'));  // ÿ
    }

    @Test
    public void unmappedCharacterReturnsNull() {
        // 'B' has no diacritical variants in this mapping
        assertNull(chars.modify(Diacritic.Grave, 'B'));
        assertNull(chars.modify(Diacritic.Acute, 'B'));
        // Numbers have no diacritical variants
        assertNull(chars.modify(Diacritic.Grave, '1'));
    }
}
