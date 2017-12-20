package net.fortytwo.smsn.brain.model;

import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Predicate;

public class Property<T, V> {
    private boolean isRequired = false;
    private boolean isSettable = true;
    private boolean isAnnotationProperty = true;
    private String key;
    private Function<T, V> getter;
    private BiConsumer<T, V> setter;
    private Function<String, V> fromString;
    private V defaultValue;
    private Class<V> valueClass;
    private Validator<V> validator;

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

    public Class<V> getValueClass() {
        return valueClass;
    }

    public boolean isRequired() {
        return isRequired;
    }

    public Validator<V> getValidator() {
        return validator;
    }

    public interface Validator<V> {
        void consume(V value) throws IllegalArgumentException;
    }

    public static class Builder<T, V> {
        private final Property<T, V> property = new Property<>();

        public Builder(final Class<V> valueClass) {
            property.valueClass = valueClass;
        }

        public Property<T, V> build() {
            checkIsReady();
            return property;
        }

        public Builder<T, V> validator(final Validator<V> validator) {
            property.validator = validator;
            return this;
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
            checkNotNull(property.valueClass, "value class");
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
