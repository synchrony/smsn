package net.fortytwo.smsn.brain.model;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.config.DataSource;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Predicate;

public class Filter implements Predicate<Atom>, Serializable {

    private static final Map<String, Float> sharabilityBySource;

    static {
        sharabilityBySource = new HashMap<>();
        for (DataSource source : SemanticSynchrony.getConfiguration().getSources()) {
            sharabilityBySource.put(source.getName(), source.getSharability());
        }
    }

    private float minSharability;
    private float minWeight;
    private float defaultWeight;
    private String defaultSource;

    private static final Filter NO_FILTER = new Filter();

    public static Filter noFilter() {
        return NO_FILTER;
    }

    private Filter() {
        // TODO: don't hard-code a source
        this(0f, 0.5f, 0f, "personal");
    }

    public Filter(final float minWeight,
                  float defaultWeight,
                  final float minSharability,
                  final String defaultSource) {

        Preconditions.checkNotNull(defaultSource);
        checkBetweenZeroAndOne(minSharability);
        checkBetweenZeroAndOne(minWeight);
        checkBetweenZeroAndOne(defaultWeight);

        Preconditions.checkArgument(defaultWeight >= minWeight, "default weight greater than minimum");

        this.minSharability = minSharability;
        this.defaultSource = defaultSource;
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

    public String getDefaultSource() {
        return defaultSource;
    }

    public float getDefaultWeight() {
        return defaultWeight;
    }

    public boolean isTrivial() {
        return minSharability == 0 && minWeight == 0;
    }

    @Override
    public boolean test(final Atom atom) {
        Float sharability = getSharability(atom);
        Float weight = atom.getWeight();

        if (null == sharability || null == weight) return false;

        // This criterion includes the minimum; if the minimum is 0.25,
        // items with a value of 0.25 and greater will be visible.
        return sharability >= minSharability && weight >= minWeight;
    }

    private Float getSharability(final Atom atom) {
        String source = atom.getSource();
        return null == source ? null : sharabilityBySource.get(source);
    }
}
