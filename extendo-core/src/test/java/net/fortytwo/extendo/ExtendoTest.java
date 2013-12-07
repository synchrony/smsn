package net.fortytwo.extendo;

import net.fortytwo.extendo.util.properties.TypedProperties;
import org.junit.Test;

import java.net.URI;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ExtendoTest {
    @Test
    public void testConfigurationProperties() throws Exception {
        TypedProperties props = Extendo.getConfiguration();

        assertEquals(new URI("http://extendo.fortytwo.net/"), props.getURI(Extendo.BASE_URI));
        assertNull(props.getString("org.example.someOtherProperty", null));
        
        props.load(ExtendoTest.class.getResourceAsStream("additional.props"));

        assertEquals(new URI("http://example.org/"), props.getURI(Extendo.BASE_URI));
        assertEquals("some other value", props.getString("org.example.someOtherProperty"));
        assertEquals(42, props.getInt("org.example.yetAnotherProperty"));
    }
    
    @Test
    public void testVersionInfo() throws Exception {
        String version = Extendo.getConfiguration().getString(Extendo.VERSION);
        int i = version.indexOf('.');
        int majorVersion = Integer.valueOf(version.substring(0, i));
        assertTrue(majorVersion >= 1);
    }
}
