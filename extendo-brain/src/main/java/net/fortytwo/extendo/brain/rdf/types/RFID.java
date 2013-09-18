package net.fortytwo.extendo.brain.rdf.types;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class RFID extends SimpleType {
    public static final RFID INSTANCE = new RFID();

    private RFID() {
    }

    public Pattern getValueRegex() {
        return Pattern.compile("RFID: [0-9A-F]{4} [0-9A-F]{4} [0-9A-F]{4} [0-9A-F]{4} [0-9A-F]{4} [0-9A-F]{4}");
    }

    public boolean fulfillsAdditionalConstraints(final String value) {
        return true;
    }
}
