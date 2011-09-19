package net.fortytwo.myotherbrain.notes;

import net.fortytwo.myotherbrain.Atom;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Filter {
    public Filter() {
        this(0f, 1f, 0.5f, 0f, 1f, 0.5f);
    }

    public Filter(final float minSharability,
                  final float maxSharability,
                  final float defaultSharability,
                  final float minWeight,
                  final float maxWeight,
                  final float defaultWeight) {
        if (minSharability < 0 || maxSharability > 1) {
            throw new IllegalArgumentException("minimum and maximum sharability must lie between 0 and 1 (inclusive)");
        }

        if (maxSharability < minSharability) {
            throw new IllegalArgumentException("maximum sharability must be greater than or equal to minimum sharability");
        }

        if (defaultSharability < minSharability || defaultSharability > maxSharability) {
            throw new IllegalArgumentException("default sharability must lie between min and max sharability");
        }

        if (minWeight < 0 || maxWeight > 1) {
            throw new IllegalArgumentException("minimum and maximum weight must lie between 0 and 1 (inclusive)");
        }

        if (maxWeight < minWeight) {
            throw new IllegalArgumentException("maximum weight must be greater than or equal to minimum weight");
        }

        if (defaultWeight < minWeight || defaultSharability > maxWeight) {
            throw new IllegalArgumentException("default weight must lie between min and max weight");
        }

        this.minSharability = minSharability;
        this.maxSharability = maxSharability;
        this.defaultSharability = defaultSharability;
        this.minWeight = minWeight;
        this.maxWeight = maxWeight;
        this.defaultWeight = defaultWeight;
    }

    public final Float minSharability, maxSharability, defaultSharability;
    public final Float minWeight, maxWeight, defaultWeight;

    public boolean isVisible(final Atom atom) {
        float sharability = atom.getSharability();
        float weight = atom.getWeight();

        // Strictly greater than the minimum, less than or equal to the maximum.
        // Values range from 0 (exclusive) to 1 (inclusive).
        return sharability > minSharability && sharability <= maxSharability
                && weight > minWeight && weight <= maxWeight;
    }

    public void makeVisible(final Atom atom) {
        float sharability = atom.getSharability();
        float weight = atom.getWeight();

        if (sharability <= minSharability || sharability > maxSharability) {
            atom.setSharability((maxSharability - minSharability) / 2);
        }

        if (weight <= minWeight || weight > maxWeight) {
            atom.setWeight((maxWeight - minWeight) / 2);
        }
    }
}
