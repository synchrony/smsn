package net.fortytwo.smsn.brain.model;

import java.util.function.BiConsumer;
import java.util.function.Function;

public class Property<T, V> {
    private boolean isRequired = false;
    private boolean isSettable = true;
    private boolean isAnnotationProperty = true;
    private String key;
    private Function<T, V> getter;
    private BiConsumer<T, V> setter;
    private Function<String, V> fromString;
    private V defaultValue;

    private Property() {}

    public boolean isAnnotationProperty() {
        return isAnnotationProperty;
    }

    public String getKey() {
        return key;
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

    public boolean isRequired() {
        return isRequired;
    }

    public static class Builder<T, V> {
        private final Property<T, V> property = new Property<>();

        public Property<T, V> build() {
            checkIsReady();

            return property;
        }

        public Builder<T, V> isRequired(final boolean isRequired) {
            property.isRequired = isRequired;
            return this;
        }

        public Builder<T, V> isSettable(final boolean isSettable) {
            property.isSettable = isSettable;
            return this;
        }

        public Builder<T, V> isAnnotationProperty(final boolean isAnnotationProperty) {
            property.isAnnotationProperty = isAnnotationProperty;
            return this;
        }

        public Builder<T, V> key(final String key) {
            property.key = key;
            return this;
        }

        public Builder<T, V> getter(final Function<T, V> getter) {
            property.getter = getter;
            return this;
        }

        public Builder<T, V> setter(final BiConsumer<T, V> setter) {
            property.setter = setter;
            return this;
        }

        public Builder<T, V> fromString(final Function<String, V> fromString) {
            property.fromString = fromString;
            return this;
        }

        public Builder<T, V> defaultValue(final V defaultValue) {
            property.defaultValue = defaultValue;
            return this;
        }

        private void checkIsReady() {
            checkNotNull(property.key, "property key");
            checkNotNull(property.getter, "getter");
            if (property.isSettable) {
                checkNotNull(property.setter, "setter");
            }
            checkNotNull(property.fromString, "from-string");
        }

        private <S> void checkNotNull(final S value, final String name) {
            if (null == value) {
                throw new IllegalArgumentException("missing " + name + " parameter");
            }
        }
    }
}
