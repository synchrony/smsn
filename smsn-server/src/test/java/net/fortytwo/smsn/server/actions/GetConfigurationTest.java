package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.ActionContext;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

public class GetConfigurationTest extends ActionTestBase {

    @Before
    public void setUp() throws Exception {
        super.setUp();
    }

    @Test
    public void returnsConfiguration() throws Exception {
        GetConfiguration action = new GetConfiguration();

        ActionContext context = perform(action);

        Object config = context.getMap().get(Params.CONFIGURATION);
        assertNotNull(config);
        assertTrue(config instanceof String);

        String configJson = (String) config;
        // Should be valid JSON containing expected fields
        assertTrue(configJson.contains("sources"));
        assertTrue(configJson.contains("activityLog"));
    }

    @Test
    public void configurationContainsSources() throws Exception {
        GetConfiguration action = new GetConfiguration();

        ActionContext context = perform(action);

        String configJson = (String) context.getMap().get(Params.CONFIGURATION);
        // Default configuration has sources like "private", "personal", etc.
        assertTrue(configJson.contains("private") || configJson.contains("public"));
    }
}
