package net.fortytwo.smsn.brain.model;

import java.util.function.BiConsumer;
import java.util.function.Function;

public class Property<T, V> {
    private final boolean isSettable;
    private final boolean isAnnotationProperty;
    private final String propertyKey;
    private final Function<T, V> getter;
    private final BiConsumer<T, V> setter;
    private final Function<String, V> fromString;
    private final V defaultValue;

    public Property(final boolean isSettable,
                     final boolean isAnnotationProperty,
                     final String propertyKey,
                     final Function<T, V> getter,
                     final BiConsumer<T, V> setter,
                     final Function<String, V> fromString,
                    final V defaultValue) {
        this.isSettable = isSettable;
        this.isAnnotationProperty = isAnnotationProperty;
        this.propertyKey = propertyKey;
        this.getter = getter;
        this.setter = setter;
        this.fromString = fromString;
        this.defaultValue = defaultValue;
    }

    public boolean isAnnotationProperty() {
        return isAnnotationProperty;
    }

    public String getPropertyKey() {
        return propertyKey;
    }

    public Function<T, V> getGetter() {
        return getter;
    }

    public BiConsumer<T, V> getSetter() {
        return setter;
    }

    public Function<String, V> getFromString() {
        return fromString;
    }

    public boolean isSettable() {
        return isSettable;
    }

    public V getDefaultValue() {
        return defaultValue;
    }
}
