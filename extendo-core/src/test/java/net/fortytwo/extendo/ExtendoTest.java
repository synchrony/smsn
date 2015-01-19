package net.fortytwo.extendo;

import net.fortytwo.extendo.util.TypedProperties;
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

        assertNull(props.getString("org.example.someOtherProperty", null));
        
        props.load(ExtendoTest.class.getResourceAsStream("additional.props"));

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

    @Test
    public void testUnicodeEscape() throws Exception {
        StringBuilder sb;

        sb = new StringBuilder();
        sb.append((char) 0);
        assertEquals("\\u0000", Extendo.unicodeEscape(sb.toString()));

        sb = new StringBuilder();
        sb.append("the number ");
        sb.append((char) 4);
        sb.append((char) 2);
        sb.append("...");
        sb.append((char) 127);
        sb.append((char) 1008);
        assertEquals("the number \\u0004\\u0002...\\u007F\\u03F0", Extendo.unicodeEscape(sb.toString()));
    }
}
