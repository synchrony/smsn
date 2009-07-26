/* Copyright (C) 2008 Knowledge Reef Systems.  All rights reserved. */

package net.fortytwo.myotherbrain.query;

/**
 * Author: josh
 * Date: Feb 1, 2008
 * Time: 12:55:40 PM
 */
public class WeightedValue<T> implements Comparable<WeightedValue<T>> {
    public double weight;
    public T value;

    public WeightedValue() {
    }

    public WeightedValue(final T value, final double weight) {
        this.value = value;
        this.weight = weight;
    }

    public int compareTo(final WeightedValue<T> other) {
        return weight < other.weight
                ? -1
                : weight > other.weight
                ? 1 : 0;
    }

    public String toString() {
        return "WeightedValue(" + weight + ", " + value + ")";
    }
}
