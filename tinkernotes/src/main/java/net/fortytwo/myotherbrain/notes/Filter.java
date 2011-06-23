package net.fortytwo.myotherbrain.notes;

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
}
