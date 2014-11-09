package net.fortytwo.extendo.typeatron;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ChordedKeyerTest {

    private ChordedKeyer keyer;
    private boolean[] keyState = new boolean[5];

    private ChordedKeyer.Mode lastMode;
    private String lastSymbol;
    private ChordedKeyer.Modifier lastModifier;

    @Before
    public void setUp() throws Exception {
        keyer = new ChordedKeyer(new ChordedKeyer.EventHandler() {
            public void handle(ChordedKeyer.Mode mode, String symbol, ChordedKeyer.Modifier modifier) {
                //System.out.println("event:\t" + mode + "\t" + symbol + "\t" + modifier);
                lastMode = mode;
                lastSymbol = symbol;
                lastModifier = modifier;
            }
        });
    }

    @Test
    public void testLettersAndPunctuation() throws Exception {
        reset();
        pressKeys(3, 2, 3, 2);
        assertEquals(ChordedKeyer.Mode.Text, lastMode);
        assertEquals("e", lastSymbol);
        assertEquals(ChordedKeyer.Modifier.None, lastModifier);

        reset();
        pressKeys(3, 2, 4, 4, 3, 2);
        assertEquals(ChordedKeyer.Mode.Text, lastMode);
        assertEquals("E", lastSymbol);
        assertEquals(ChordedKeyer.Modifier.None, lastModifier);

        reset();
        pressKeys(3, 2, 1, 1, 3, 2);
        assertEquals(ChordedKeyer.Mode.Text, lastMode);
        assertEquals("e", lastSymbol);
        assertEquals(ChordedKeyer.Modifier.Control, lastModifier);

        reset();
        pressKeys(3, 2, 5, 5, 3, 2);
        assertEquals(ChordedKeyer.Mode.Text, lastMode);
        assertEquals("=", lastSymbol);
        assertEquals(ChordedKeyer.Modifier.None, lastModifier);

        // make sure the comma has been read correctly from the CSV
        reset();
        pressKeys(3, 1, 5, 5, 1, 3);
        assertEquals(ChordedKeyer.Mode.Text, lastMode);
        assertEquals(",", lastSymbol);
        assertEquals(ChordedKeyer.Modifier.None, lastModifier);
    }

    @Test
    public void testWhitespace() throws Exception {
        // TODO: 1,1 is unmapped

        reset();
        pressKeys(2, 2);
        assertEquals(ChordedKeyer.Mode.Text, lastMode);
        assertEquals(" ", lastSymbol);
        assertEquals(ChordedKeyer.Modifier.None, lastModifier);

        reset();
        pressKeys(3, 3);
        assertEquals(ChordedKeyer.Mode.Text, lastMode);
        assertEquals("\n", lastSymbol);
        assertEquals(ChordedKeyer.Modifier.None, lastModifier);

        reset();
        pressKeys(4, 4);
        assertEquals(ChordedKeyer.Mode.Text, lastMode);
        assertEquals("DEL", lastSymbol);
        assertEquals(ChordedKeyer.Modifier.None, lastModifier);

        reset();
        pressKeys(5, 5);
        assertEquals(ChordedKeyer.Mode.Text, lastMode);
        assertEquals("ESC", lastSymbol);
        assertEquals(ChordedKeyer.Modifier.None, lastModifier);
    }

    private void reset() {
        lastMode = null;
        lastSymbol = null;
        lastModifier = null;
    }

    private void pressKeys(final int... keys) {
        byte[] inputState = new byte[5];
        for (int i = 0; i < 5; i++) {
            inputState[i] = (byte) (keyState[i] ? '1' : '0');
        }

        for (int key : keys) {
            int index = key - 1;
            keyState[index] = !keyState[index];
            inputState[index] = (byte) (keyState[index] ? '1' : '0');

            //System.out.println("input: " + new String(inputState));
            keyer.nextInputState(inputState);
        }
    }
}
