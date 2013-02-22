package net.fortytwo.myotherbrain;

import net.fortytwo.myotherbrain.util.properties.TypedProperties;
import org.junit.Test;

import java.net.URI;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class MyOtherBrainTest {
    @Test
    public void testConfigurationProperties() throws Exception {
        TypedProperties props = MyOtherBrain.getConfiguration();

        assertEquals(new URI("http://myotherbrain.fortytwo.net/"), props.getURI(MyOtherBrain.BASE_URI));
        assertNull(props.getString("org.example.someOtherProperty", null));
        
        props.load(MyOtherBrainTest.class.getResourceAsStream("additional.props"));

        assertEquals(new URI("http://example.org/"), props.getURI(MyOtherBrain.BASE_URI));
        assertEquals("some other value", props.getString("org.example.someOtherProperty"));
        assertEquals(42, props.getInt("org.example.yetAnotherProperty"));
    }
    
    @Test
    public void testVersionInfo() {
        assertEquals("MyOtherBrain 0.2-SNAPSHOT, revision #-1", MyOtherBrain.getVersionInfo());
    }
}
