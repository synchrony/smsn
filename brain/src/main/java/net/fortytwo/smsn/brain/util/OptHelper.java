package net.fortytwo.smsn.brain.util;

import hydra.util.Opt;

/**
 * Helper class to work with Opt values without requiring importing Opt in other modules.
 * This is needed because some modules (like smsn-server) don't have direct access to hydra.util.
 */
public class OptHelper {

    public static <T> boolean isPresent(Opt<T> opt) {
        return opt.isPresent();
    }

    public static <T> T get(Opt<T> opt) {
        return opt.get();
    }

    public static <T> T getOrNull(Opt<T> opt) {
        return opt.isPresent() ? opt.get() : null;
    }

    public static String getOrEmpty(Opt<String> opt) {
        return opt.isPresent() ? opt.get() : "";
    }
}
