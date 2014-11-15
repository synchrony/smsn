package net.fortytwo.extendo.brain;

import groovy.json.StringEscapeUtils;
import junit.framework.TestCase;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BrainModeClientTest extends TestCase {

    private final List<String> results = new ArrayList<String>();

    private final BrainModeClient.EmacsFunctionExecutor functionExecutor = new BrainModeClient.EmacsFunctionExecutor() {
        public Process execute(BrainModeClient.EmacsFunction function, String argument)
                throws InterruptedException, IOException {

            String expr = function.getRequiresArgument()
                    ? "(" + function.getName() + " \"" + StringEscapeUtils.escapeJava(argument) + "\")"
                    : "(" + function.getName() + ")";

            results.add(expr);
            return null;
        }
    };

    @Test
    public void testAll() throws Exception {
        // go up and down, and type some text (note: the 'e' is not a typo)
        assertExpected("<down><up><C-c><C-v>esome inserted text<down>",
                "(exo-next-line)",
                "(exo-previous-line)",
                "(exo-enter-edit-view)",
                "(insert \"some inserted text\")",
                "(exo-next-line)");

        // search for the "sandbox" note and select it from the search results,
        // visit the 11th child of sandbox and add a new note to it
        assertExpected("<C-c>ssandb*\n<down><C-c>t<C-c><C-l>aa\n<C-c>t<C-c><C-v>e* here is a new note\n<C-c>p",
                "(exo-search \"sandb*\")",
                "(exo-next-line)",
                "(exo-visit-target)",
                "(exo-goto-line \"aa\")",
                "(exo-visit-target)",
                "(exo-enter-edit-view)",
                "(insert \"* here is a new note\\n\")",
                "(exo-push-view)");

        assertExpected("<C-c><C-e>l",
                "(emacspeak-speak-line)");
    }

    private void assertExpected(final String input,
                                final String... outputs) throws Exception {
        results.clear();

        InputStream in = new ByteArrayInputStream(input.getBytes());
        BrainModeClient.ResultHandler handler = new BrainModeClient.ResultHandler() {
            @Override
            public void handle(InputStream result) {
                // ignore
            }
        };
        BrainModeClient client = new BrainModeClient(in, handler);
        client.setFunctionExecutor(functionExecutor);

        client.run();

        int min = Math.min(results.size(), outputs.length);
        for (int i = 0; i < min; i++) {
            assertEquals("unexpected output at index " + (i + 1), outputs[i], results.get(i));
        }

        if (results.size() < outputs.length) {
            assertTrue("insufficient output: expecting " + outputs[results.size()]
                    + " at index " + results.size(), false);
        } else if (results.size() > outputs.length) {
            assertTrue("excess output beginning with " + results.get(outputs.length)
                    + " at index " + outputs.length, false);
        }
    }
}
