package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.rdf.SimpleNoteClass;

import java.util.regex.Pattern;

public class RFIDReference extends SimpleNoteClass {

    public RFIDReference() {
        super(
                "rfid",
                Pattern.compile("RFID: [0-9A-F]{4} [0-9A-F]{4} [0-9A-F]{4} [0-9A-F]{4} [0-9A-F]{4} [0-9A-F]{4}"),
                null,
                null
                );
    }
}
