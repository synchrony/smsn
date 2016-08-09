package net.fortytwo.smsn.typeatron.ripple;

import net.fortytwo.smsn.typeatron.TypeatronControl;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

public class UserDictionary {

    private static final Logger logger = Logger.getLogger(UserDictionary.class.getName());

    private final Map<String, Object> map = new HashMap<>();
    private final TypeatronControl typeatron;

    private UserDictionary(TypeatronControl typeatron) {
        this.typeatron = typeatron;
    }

    public Object get(final String symbol) {
        return map.get(symbol);
    }

    public Object put(final String symbol,
                      final Object value) {
        Object existing = map.get(symbol);
        if (null == existing) {
            map.put(symbol, value);
            return value;
        } else {
            logger.info("new value " + value + " rejected for symbol " + symbol
                    + " due to pre-existing value " + existing);

            // alert the user to the "exception"
            typeatron.sendWarningMessage();

            return existing;
        }
    }

    public void remove(final String symbol) {
        map.remove(symbol);
    }
}
