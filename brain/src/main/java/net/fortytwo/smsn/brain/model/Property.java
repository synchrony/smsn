package net.fortytwo.smsn.brain.model;

import java.util.function.BiConsumer;
import java.util.function.Function;

public class Property<T, V> {
    private boolean isSettable;
    private boolean isAnnotationProperty;
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

    public static class Builder<T, V> {
        private boolean isSettable = true;
        private boolean isAnnotationProperty = true;
        private String key;
        private Function<T, V> getter;
        private BiConsumer<T, V> setter;
        private Function<String, V> fromString;
        private V defaultValue;

        public Property<T, V> build() {
            checkIsReady();

            Property<T, V> property = new Property<>();
            property.isSettable = isSettable;
            property.isAnnotationProperty = isAnnotationProperty;
            property.key = key;
            property.getter = getter;
            property.setter = setter;
            property.fromString = fromString;
            property.defaultValue = defaultValue;

            return property;
        }

        public Builder<T, V> isSettable(final boolean isSettable) {
            this.isSettable = isSettable;
            return this;
        }

        public Builder<T, V> isAnnotationProperty(final boolean isAnnotationProperty) {
            this.isAnnotationProperty = isAnnotationProperty;
            return this;
        }

        public Builder<T, V> key(final String key) {
            this.key = key;
            return this;
        }

        public Builder<T, V> getter(final Function<T, V> getter) {
            this.getter = getter;
            return this;
        }

        public Builder<T, V> setter(final BiConsumer<T, V> setter) {
            this.setter = setter;
            return this;
        }

        public Builder<T, V> fromString(final Function<String, V> fromString) {
            this.fromString = fromString;
            return this;
        }

        public Builder<T, V> defaultValue(final V defaultValue) {
            this.defaultValue = defaultValue;
            return this;
        }

        private void checkIsReady() {
            checkNotNull(key, "property key");
            checkNotNull(getter, "getter");
            if (isSettable) {
                checkNotNull(setter, "setter");
            }
            checkNotNull(fromString, "from-string");
        }

        private <S> void checkNotNull(final S value, final String name) {
            if (null == value) {
                throw new IllegalArgumentException("missing " + name + " parameter");
            }
        }
    }
}
