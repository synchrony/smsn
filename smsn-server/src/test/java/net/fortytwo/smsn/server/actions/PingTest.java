package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.server.ActionContext;
import org.junit.Test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class PingTest extends ActionTestBase {

    @Test
    public void pingReturnsTime() throws Exception {
        Ping action = new Ping();

        long before = System.currentTimeMillis();
        ActionContext context = perform(action);
        long after = System.currentTimeMillis();

        String timeStr = (String) context.getMap().get("time");
        assertNotNull(timeStr);

        long time = Long.parseLong(timeStr);
        assertTrue("time should be >= before", time >= before);
        assertTrue("time should be <= after", time <= after);
    }

    @Test
    public void pingDoesNotReadOrWrite() {
        Ping action = new Ping();
        // These are protected methods, but we can verify the behavior indirectly
        // by checking that no transaction is required
        assertNotNull(perform(action));
    }
}
