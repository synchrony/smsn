package net.fortytwo.extendo.brain;

import com.tinkerpop.blueprints.Vertex;
import net.fortytwo.extendo.Extendo;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Filter {

    private final Float minSharability, maxSharability;
    private final Float minWeight, maxWeight;

    private final Float defaultSharability, defaultWeight;

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
            throw new IllegalArgumentException("maximum sharability must be greater than or equal to minimum sharability");
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

    public Float getMinSharability() {
        return minSharability;
    }

    public Float getMaxSharability() {
        return maxSharability;
    }

    public Float getMinWeight() {
        return minWeight;
    }

    public Float getMaxWeight() {
        return maxWeight;
    }

    public Float getDefaultSharability() {
        return defaultSharability;
    }

    public Float getDefaultWeight() {
        return defaultWeight;
    }

    public boolean isVisible(final Vertex atomVertex) {
        if (null == atomVertex) {
            throw new IllegalArgumentException();
        }

        Float sharability = atomVertex.getProperty(Extendo.SHARABILITY);
        if (null == sharability) {
            Extendo.LOGGER.warning("atom " + atomVertex.getId() + " has no @sharability");
            return false;
        }

        Float weight = atomVertex.getProperty(Extendo.WEIGHT);
        if (null == weight) {
            Extendo.LOGGER.warning("atom " + atomVertex.getId() + " has no @weight");
            return false;
        }

        // Strictly greater than the minimum, less than or equal to the maximum.
        // Values range from 0 (exclusive) to 1 (inclusive).
        return sharability > minSharability && sharability <= maxSharability
                && weight > minWeight && weight <= maxWeight;
    }
}
