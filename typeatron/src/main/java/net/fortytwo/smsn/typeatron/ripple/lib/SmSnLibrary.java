package net.fortytwo.smsn.typeatron.ripple.lib;

import net.fortytwo.smsn.typeatron.ripple.NoteType;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.Library;
import net.fortytwo.ripple.model.LibraryLoader;
import net.fortytwo.ripple.model.Model;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SmSnLibrary extends Library {
    public static final String
            // note: was http://fortytwo.net/2014/12/extendo#
            NS_2014_12 = "http://fortytwo.net/2014/12/smsn#";

    public void load(LibraryLoader.Context context) throws RippleException {
        Model model = context.getModelConnection().getModel();
        model.register(new NoteType());

        // note: primitives are currently registered in TypeatronDictionaryMapping
    }
}
