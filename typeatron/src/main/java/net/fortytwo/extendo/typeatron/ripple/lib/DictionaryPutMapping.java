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
public class DictionaryPutMapping extends PrimitiveStackMapping {
    private final UserDictionary dictionary;

    public DictionaryPutMapping(final UserDictionary dictionary) {
        this.dictionary = dictionary;
    }

    public String[] getIdentifiers() {
        return new String[]{
                BrainstemLibrary.NS_2014_04 + "shortcut-put"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{
                new Parameter("name", "the key of the shortcut", true),
                new Parameter("value", "the value to be stored", true)
        };
    }

    public String getComment() {
        return "associates a value with a key in the shortcut dictionary";
    }

    public void apply(final RippleList arg,
                      final Sink<RippleList> solutions,
                      final ModelConnection context) throws RippleException {
        String key = context.toString(arg.getFirst());
        RippleList rest = arg.getRest();
        RippleValue value = rest.getFirst();

        RippleValue newValue = dictionary.put(key, value);

        solutions.put(rest.getRest().push(newValue));
    }
}
