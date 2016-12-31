package net.fortytwo.smsn.brain.model;

import java.io.Serializable;

public class Filter implements Serializable {

    private float minSharability, maxSharability;
    private float minWeight, maxWeight;
    private float defaultSharability, defaultWeight;

    public Filter() {
        this(0f, 1f, 0.5f, 0f, 1f, 0.5f);
    }

    public Filter(final float minWeight,
                  final float maxWeight,
                  float defaultWeight,
                  final float minSharability,
                  final float maxSharability,
                  float defaultSharability) {
        if (minSharability < 0 || maxSharability > 1) {
            throw new IllegalArgumentException("minimum and maximum sharability must lie between 0 and 1 (inclusive)");
        }

        if (maxSharability < minSharability) {
            throw new IllegalArgumentException(
                    "maximum sharability must be greater than or equal to minimum sharability");
        }

        if (defaultSharability <= 0) {
            defaultSharability = (maxSharability + minSharability) / 2f;
        } else if (defaultSharability < minSharability || defaultSharability > maxSharability) {
            throw new IllegalArgumentException("default sharability must lie between min and max sharability");
        }

        if (minWeight < 0 || maxWeight > 1) {
            throw new IllegalArgumentException("minimum and maximum weight must lie between 0 and 1 (inclusive)");
        }

        if (maxWeight < minWeight) {
            throw new IllegalArgumentException("maximum weight must be greater than or equal to minimum weight");
        }

        if (defaultWeight <= 0) {
            defaultWeight = (maxWeight + minWeight) / 2f;
        } else if (defaultWeight < minWeight || defaultSharability > maxWeight) {
            throw new IllegalArgumentException("default weight must lie between min and max weight");
        }

        this.minSharability = minSharability;
        this.maxSharability = maxSharability;
        this.defaultSharability = defaultSharability;
        this.minWeight = minWeight;
        this.maxWeight = maxWeight;
        this.defaultWeight = defaultWeight;
    }

    public float getMinSharability() {
        return minSharability;
    }

    public float getMaxSharability() {
        return maxSharability;
    }

    public float getMinWeight() {
        return minWeight;
    }

    public float getMaxWeight() {
        return maxWeight;
    }

    public float getDefaultSharability() {
        return defaultSharability;
    }

    public float getDefaultWeight() {
        return defaultWeight;
    }

    public void setMinSharability(float minSharability) {
        this.minSharability = minSharability;
    }

    public void setMaxSharability(float maxSharability) {
        this.maxSharability = maxSharability;
    }

    public void setMinWeight(float minWeight) {
        this.minWeight = minWeight;
    }

    public void setMaxWeight(float maxWeight) {
        this.maxWeight = maxWeight;
    }

    public void setDefaultSharability(float defaultSharability) {
        this.defaultSharability = defaultSharability;
    }

    public void setDefaultWeight(float defaultWeight) {
        this.defaultWeight = defaultWeight;
    }

    public boolean isTrivial() {
        return minSharability == 0 && minWeight == 0;
    }

    public boolean isVisible(final Atom atom) {
        float sharability = atom.getSharability();
        float weight = atom.getWeight();

        // Strictly greater than the minimum, less than or equal to the maximum.
        // Values range from 0 (exclusive) to 1 (inclusive).
        return sharability > minSharability && sharability <= maxSharability
                && weight > minWeight && weight <= maxWeight;
    }
}
