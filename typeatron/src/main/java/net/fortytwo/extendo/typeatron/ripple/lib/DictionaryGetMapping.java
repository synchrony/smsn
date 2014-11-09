package net.fortytwo.extendo.typeatron.ripple.lib;

import net.fortytwo.extendo.typeatron.ripple.UserDictionary;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.PrimitiveStackMapping;
import net.fortytwo.ripple.model.RippleList;
import net.fortytwo.ripple.model.RippleValue;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class DictionaryGetMapping extends PrimitiveStackMapping {
    private final UserDictionary dictionary;

    public DictionaryGetMapping(UserDictionary dictionary) {
            this.dictionary = dictionary;
    }

    public String[] getIdentifiers() {
        return new String[]{
                BrainstemLibrary.NS_2014_04 + "shortcut-get"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{
                new Parameter("name", "the key of the shortcut", true),
        };
    }

    public String getComment() {
        return "gets a value associated with a key in the shortcut dictionary";
    }

    public void apply(final RippleList arg,
                      final Sink<RippleList> solutions,
                      final ModelConnection context) throws RippleException {

        String key = context.toString(arg.getFirst());

        RippleValue value = dictionary.get(key);
        if (null == value) {
            value = context.valueOf("");
        }

        solutions.put(arg.getRest().push(value));
    }
}
