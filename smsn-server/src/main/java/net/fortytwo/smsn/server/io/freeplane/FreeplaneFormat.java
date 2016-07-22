package net.fortytwo.smsn.server.io.freeplane;

import net.fortytwo.smsn.server.io.Format;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class FreeplaneFormat extends Format {
    private static final FreeplaneFormat instance = new FreeplaneFormat();

    private FreeplaneFormat() {
        super("Freeplane", new String[]{"mm"});
    }

    public static FreeplaneFormat getInstance() {
        return instance;
    }
}
