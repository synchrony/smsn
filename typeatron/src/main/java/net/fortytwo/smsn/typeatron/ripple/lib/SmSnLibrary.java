package net.fortytwo.smsn.typeatron.ripple.lib;

import net.fortytwo.smsn.typeatron.ripple.TreeType;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.Library;
import net.fortytwo.ripple.model.LibraryLoader;
import net.fortytwo.ripple.model.Model;

public class SmSnLibrary extends Library {
    public static final String
            // was: http://fortytwo.net/2009/05/extendobrain#
            // was: http://fortytwo.net/2014/12/extendo#
            NS_2014_12 = "http://fortytwo.net/2014/12/smsn#";

    public void load(LibraryLoader.Context context) throws RippleException {
        Model model = context.getModelConnection().getModel();
        model.register(new TreeType());

        // note: primitives are currently registered in TypeatronDictionaryMapping
    }
}
