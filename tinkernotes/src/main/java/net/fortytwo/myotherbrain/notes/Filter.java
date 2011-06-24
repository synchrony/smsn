package net.fortytwo.myotherbrain.notes;

import net.fortytwo.myotherbrain.Atom;

/**
 * User: josh
 * Date: 6/24/11
 * Time: 12:22 AM
 */
public class Filter {
    public Filter(final float minVisibility,
                  final float maxVisibility,
                  final float minWeight,
                  final float maxWeight) {
        this.minVisibility = minVisibility;
        this.maxVisibility = maxVisibility;
        this.minWeight = minWeight;
        this.maxWeight = maxWeight;
    }

    public final float minVisibility, maxVisibility;
    public final float minWeight, maxWeight;

    public boolean isVisible(final Atom atom) {
        float visibility = atom.getVisibility();
        float weight = atom.getWeight();

        // Strictly greater than the minimum, less than or equal to the maximum.
        // Values range from 0 (exclusive) to 1 (inclusive).
        return visibility > minVisibility && visibility <= maxVisibility
                && weight > minWeight && weight <= maxWeight;
    }

    public void makeVisible(final Atom atom) {
        float visibility = atom.getVisibility();
        float weight = atom.getWeight();

        if (visibility <= minVisibility || visibility > maxVisibility) {
            atom.setVisibility((maxVisibility - minVisibility) / 2);
        }

        if (weight <= minWeight || weight > maxWeight) {
            atom.setWeight((maxWeight - minWeight) / 2);
        }
    }
}
