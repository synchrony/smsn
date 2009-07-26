/* Copyright (C) 2008 Knowledge Reef Systems.  All rights reserved. */

package net.fortytwo.myotherbrain.query;

import net.fortytwo.myotherbrain.query.Handler;
import net.fortytwo.myotherbrain.query.WeightedValue;

import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Author: josh
 * Date: Feb 1, 2008
 * Time: 12:57:06 PM
 */
public class WeightedVector<T> {
    protected Map<T, WeightedValue<T>> valueToWeightedValue;

    public WeightedVector() {
        valueToWeightedValue = new HashMap<T, WeightedValue<T>>();
    }

    public WeightedVector(final WeightedVector<T> other) {
        valueToWeightedValue = new HashMap<T, WeightedValue<T>>(other.valueToWeightedValue);
    }

    protected WeightedVector(final Map<T, WeightedValue<T>> map) {
        valueToWeightedValue = map;
    }

    public int size() {
        return valueToWeightedValue.size();
    }

    public Set<T> keySet() {
        return valueToWeightedValue.keySet();
    }

    public Collection<WeightedValue<T>> values() {
        return valueToWeightedValue.values();
    }

    public double getWeight(final T v) {
        WeightedValue<T> wv = valueToWeightedValue.get(v);
        return (null == wv) ? 0 : wv.weight;
    }

    public void setWeight(final T v, final double weight) {
        if (0 == weight) {
            valueToWeightedValue.remove(v);
        } else {
            WeightedValue<T> wv = valueToWeightedValue.get(v);

            if (null == wv) {
                wv = new WeightedValue<T>();
                wv.value = v;
//System.out.println("    put(" + v + ", " + wv + ")");
                valueToWeightedValue.put(v, wv);
            }

            wv.weight = weight;
//System.out.println("    wv.weight = " + wv.weight);
//System.out.println("    retrieved: " + getWeight(v));
//System.out.println("    size = " + size());
        }
    }

    // Convenience method.
    public void addWeight(final T v, final double weight) {
        WeightedValue<T> wv = valueToWeightedValue.get(v);

        if (null == wv) {
            wv = new WeightedValue<T>();
            wv.value = v;
            wv.weight = weight;
            valueToWeightedValue.put(v, wv);
        } else {
            wv.weight += weight;
            if (0 == wv.weight) {
                valueToWeightedValue.remove(v);
            }
        }
    }

    public Handler<WeightedValue<T>, Exception> createAddValueSink() {
        return new Handler<WeightedValue<T>, Exception>() {
            public boolean handle(final WeightedValue<T> wv) throws Exception {
                T v = wv.value;
                double w1 = getWeight(v);
                double w2 = wv.weight;

                setWeight(v, w1 + w2);
                return true;
            }
        };
    }

    public Handler<WeightedValue<T>, Exception> createSubtractValueSink() {
        return new Handler<WeightedValue<T>, Exception>() {
            public boolean handle(final WeightedValue<T> wv) throws Exception {
                T v = wv.value;
                double w1 = getWeight(v);
                double w2 = wv.weight;

                setWeight(v, w1 - w2);
                return true;
            }
        };
    }

    /**
     * @return the vector as an unordered array of weighted values
     */
    public WeightedValue<T>[] toArray() {
        WeightedValue<T>[] wvalues = new WeightedValue[valueToWeightedValue.size()];
//System.out.println("valueToWeightedValue.size() = " + valueToWeightedValue.size());
        valueToWeightedValue.values().toArray(wvalues);
        return wvalues;
    }

    /**
     * @return the vector as an array of weighted values, sorted in order of
     *         decreasing weight
     */
    public WeightedValue<T>[] toSortedArray() {
        WeightedValue<T>[] wvalues = new WeightedValue[valueToWeightedValue.size()];
//System.out.println("valueToWeightedValue.size() = " + valueToWeightedValue.size());
        valueToWeightedValue.values().toArray(wvalues);

        Comparator<WeightedValue> comp = new Comparator<WeightedValue>() {
            public int compare(final WeightedValue wv1, final WeightedValue wv2) {
                return wv1.weight < wv2.weight ? 1 : wv1.weight == wv2.weight ? 0 : -1;
            }
        };

        Arrays.sort(wvalues, comp);

        return wvalues;
    }

    public void clear() {
        valueToWeightedValue.clear();
    }

    /**
     * @return magnitude of this vector
     */
    public double getMagnitude() {
        double sumOfSquares = 0;

        for (WeightedValue<T> wv : valueToWeightedValue.values()) {
            sumOfSquares += (wv.weight *= wv.weight);
        }

        return Math.sqrt(sumOfSquares);
    }

    /**
     * @param c
     * @return product of this vector with a constant value
     */
    public WeightedVector<T> multiplyBy(final double c) {
        WeightedVector<T> result = new WeightedVector<T>(this);

        for (WeightedValue<T> wv : valueToWeightedValue.values()) {
            result.setWeight(wv.value, wv.weight * c);
        }

        return result;
    }

    /**
     * @param other
     * @return sum of this vector with another vector (set union)
     */
    public WeightedVector<T> add(final WeightedVector<T> other) {
        WeightedVector<T> result = new WeightedVector<T>(this);

        for (WeightedValue<T> wv : other.valueToWeightedValue.values()) {
            T v = wv.value;
            double w1 = getWeight(v);
            double w2 = wv.weight;

//System.out.println("setting weight of " + v + " to " + (w1 + w2) + ".");
            result.setWeight(v, w1 + w2);
        }

//System.out.println("result.size() = " + result.size());
        return result;
    }

    /**
     * @param other
     * @return result of subtracting another vector from this
     *         vector (set exclusion)
     */
    public WeightedVector<T> subtract(final WeightedVector<T> other) {
        WeightedVector<T> result = new WeightedVector<T>(this);

        for (WeightedValue<T> wv : other.valueToWeightedValue.values()) {
            T v = wv.value;
            double w1 = getWeight(v);
            double w2 = wv.weight;

            result.setWeight(v, w1 - w2);
        }

        return result;
    }

    /**
     * @param other
     * @return dot product of this vector with another vector
     */
    public double dotMultiplyBy(final WeightedVector<T> other) {
        double result = 0;

        for (WeightedValue<T> wv : valueToWeightedValue.values()) {
            T v = wv.value;
            double w1 = wv.weight;
            double w2 = other.getWeight(v);

            result += (w1 * w2);
        }

        return result;
    }

    /**
     * @param other
     * @return product of this vector with the transpose of another vector
     *         (set intersection)
     */
    public WeightedVector<T> multiplyByTransposeOf(final WeightedVector<T> other) {
        WeightedVector<T> result = new WeightedVector<T>(this);

        for (WeightedValue<T> wv : valueToWeightedValue.values()) {
            T v = wv.value;
            double w1 = wv.weight;
            double w2 = other.getWeight(v);

            result.setWeight(v, w1 * w2);
        }

        return result;
    }
}
