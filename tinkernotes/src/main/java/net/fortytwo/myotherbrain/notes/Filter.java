package net.fortytwo.myotherbrain.notes;

import net.fortytwo.myotherbrain.Atom;

/**
 * User: josh
 * Date: 6/24/11
 * Time: 12:22 AM
 */
public class Filter {
    public Filter() {
        this(0f, 1f, 0f, 1f);
    }

    public Filter(final float minSharability,
                  final float maxSharability,
                  final float minWeight,
                  final float maxWeight) {
        this.minSharability = minSharability;
        this.maxSharability = maxSharability;
        this.minWeight = minWeight;
        this.maxWeight = maxWeight;
    }

    public final float minSharability, maxSharability;
    public final float minWeight, maxWeight;

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
