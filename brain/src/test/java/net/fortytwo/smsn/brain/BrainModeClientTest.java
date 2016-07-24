package net.fortytwo.smsn.brain;

import org.apache.commons.lang.StringUtils;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BrainModeClientTest {

    private final List<String> results = new ArrayList<>();

    private final BrainModeClient.EmacsFunctionExecutor functionExecutor = (function, argument) -> {
        String expr = function.getRequiresArgument()
                ? "(" + function.getName() + " \"" + StringUtils.escape(argument) + "\")"
                : "(" + function.getName() + ")";

        results.add(expr);
        return null;
    };

    @Test
    public void testAll() throws Exception {
        // go up and down, and type some text (note: the 'e' is not a typo)
        assertExpected("<down><up><C-c><C-v>esome inserted text<down>",
                "(smsn-next-line)",
                "(smsn-previous-line)",
                "(smsn-enter-edit-view)",
                "(insert \"some inserted text\")",
                "(smsn-next-line)");

        // search for the "sandbox" note and select it from the search results,
        // visit the 11th child of sandbox and add a new note to it
        assertExpected("<C-c>ssandb*\n<down><C-c>t<C-c><C-l>aa\n<C-c>t<C-c><C-v>e* here is a new note\n<C-c>p",
                "(smsn-fulltext-query \"sandb*\")",
                "(smsn-next-line)",
                "(smsn-visit-target)",
                "(smsn-goto-line \"aa\")",
                "(smsn-visit-target)",
                "(smsn-enter-edit-view)",
                "(insert \"* here is a new note\\n\")",
                "(smsn-push-view)");

        assertExpected("<C-c><C-e>l",
                "(emacspeak-speak-line)");
    }

    private void assertExpected(final String input,
                                final String... outputs) throws Exception {
        results.clear();

        InputStream in = new ByteArrayInputStream(input.getBytes());
        BrainModeClient.ResultHandler handler = result -> {
            // ignore
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
