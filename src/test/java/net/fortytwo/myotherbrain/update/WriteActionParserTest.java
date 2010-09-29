package net.fortytwo.myotherbrain.update;

import net.fortytwo.myotherbrain.update.actions.WriteActionTestCase;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * User: josh
 * Date: Sep 29, 2010
 * Time: 10:26:01 PM
 */
public class WriteActionParserTest extends WriteActionTestCase {
    private WriteActionParser parser = new WriteActionParser();

    public void testAll() throws Exception {
        String s = "{" +
                "  action: 'addMarkerTag'," +
                "  params: {" +
                "    subject: 'http://example.org/ns/foo'," +
                "    newMarkerTag: 'http://example.org/ns/bar'," +
                "  }" +
                "}";

        WriteAction a = parse(s, context);
    }

    public void testNonexistentAction() throws Exception {
        String s = "{" +
                "  action: 'bogusAction'," +
                "  params: {" +
                "  }" +
                "}";

        boolean caught = false;
        try {
            parse(s, context);
        } catch (WriteActionParseException e) {
            caught = true;
        }

        assertTrue(caught);
    }

    public void testMissingField() throws Exception {
        String s = "{" +
                "  action: 'addMarkerTag'," +
                "  params: {" +
                "  }" +
                "}";

        boolean caught = false;
        try {
            parse(s, context);
        } catch (WriteActionParseException e) {
            caught = true;
        }

        assertTrue(caught);
    }

    public void testNonexistentField() throws Exception {
        // TODO
    }

    public void testInvalidFieldValue() throws Exception {
        // TODO
    }

    private WriteAction parse(final String s,
                              final WriteContext c) throws JSONException, UpdateException {
        JSONObject o = new JSONObject(s);
        return parser.parse(o, c);
    }
}
