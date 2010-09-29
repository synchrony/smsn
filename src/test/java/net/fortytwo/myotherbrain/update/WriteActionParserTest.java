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
        WriteContext c = new WriteContext(model.createConnection());

        String s = "{" +
                "  action: 'addMarkerTag'," +
                "  params: {" +
                "    blah: ..." +
                "  }" +
                "}";

        WriteAction a = parse(s, c);

        c.getConnection().rollback();
        c.getConnection().close();
    }

    public void testNonexistentAction() throws Exception {
        // TODO
    }

    public void testMissingField() throws Exception {
        // TODO
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
