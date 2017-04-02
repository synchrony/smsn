package net.fortytwo.smsn.brain.model;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.brain.model.entities.Atom;

import java.io.Serializable;
import java.util.function.Predicate;

public class Filter implements Predicate<Atom>, Serializable {

    private float minSharability;
    private float minWeight;
    private float defaultSharability, defaultWeight;

    private static final Filter NO_FILTER = new Filter();

    public static Filter noFilter() {
        return NO_FILTER;
    }

    private Filter() {
        this(0f, 0.5f, 0f, 0.5f);
    }

    public Filter(final float minWeight,
                  float defaultWeight,
                  final float minSharability,
                  float defaultSharability) {

        checkBetweenZeroAndOne(minSharability);
        checkBetweenZeroAndOne(defaultSharability);
        checkBetweenZeroAndOne(minWeight);
        checkBetweenZeroAndOne(defaultWeight);

        Preconditions.checkArgument(defaultSharability >= minSharability, "default sharability greater than minimum");
        Preconditions.checkArgument(defaultWeight >= minWeight, "default weight greater than minimum");

        this.minSharability = minSharability;
        this.defaultSharability = defaultSharability;
        this.minWeight = minWeight;
        this.defaultWeight = defaultWeight;
    }

    private void checkBetweenZeroAndOne(final float value) {
        Preconditions.checkArgument(value >= 0f && value <= 1f, "argument outside of range [0, 1]");
    }

    public float getMinSharability() {
        return minSharability;
    }

    public float getMinWeight() {
        return minWeight;
    }

    public float getDefaultSharability() {
        return defaultSharability;
    }

    public float getDefaultWeight() {
        return defaultWeight;
    }

    public boolean isTrivial() {
        return minSharability == 0 && minWeight == 0;
    }

    @Override
    public boolean test(final Atom atom) {
        float sharability = atom.getSharability();
        float weight = atom.getWeight();

        // This criterion includes the minimum; if the minimum is 0.25,
        // items with a value of 0.25 and greater will be visible.
        return sharability >= minSharability && weight >= minWeight;
    }
}
