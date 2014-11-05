package net.fortytwo.extendo.typeatron.ripple;

import net.fortytwo.extendo.typeatron.TypeatronControl;
import net.fortytwo.ripple.model.RippleValue;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class UserDictionary {
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
            // alert the user to the "exception"
            typeatron.sendVibrateCommand(TypeatronControl.VIBRATE_ALERT_MS);

            return existing;
        }
    }

    public void remove(final String symbol) {
        map.remove(symbol);
    }
}
