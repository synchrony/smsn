package net.fortytwo.extendo.typeatron.ripple;

import net.fortytwo.extendo.typeatron.TypeatronControl;
import net.fortytwo.ripple.model.RippleValue;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class UserDictionary {

    private static final Logger logger = Logger.getLogger(UserDictionary.class.getName());

    private Map<String, RippleValue> map = new HashMap<String, RippleValue>();
    private final TypeatronControl typeatron;

    public UserDictionary(TypeatronControl typeatron) {
        this.typeatron = typeatron;
    }

    public RippleValue get(final String symbol) {
        return map.get(symbol);
    }

    public RippleValue put(final String symbol,
                           final RippleValue value) {
        RippleValue existing = map.get(symbol);
        if (null == existing) {
            map.put(symbol, value);
            return value;
        } else {
            logger.info("new value " + value + " rejected for symbol " + symbol
                    + " due to pre-existing value " + existing);

            // alert the user to the "exception"
            typeatron.sendWarningCue();

            return existing;
        }
    }

    public void remove(final String symbol) {
        map.remove(symbol);
    }
}
