package net.fortytwo.smsn.brain.model;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.config.DataSource;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;

public class Filter implements Predicate<Note>, Serializable {

    private float minWeight;
    private float defaultWeight;
    private Set<String> includedSources;
    private String defaultSource;

    private static final Filter NO_FILTER = new Filter();

    private static final Filter SIMPLE_FILTER = new Filter(0f, 0.5f, Collections.emptySet(), null);

    public static Filter noFilter() {
        return NO_FILTER;
    }

    public static Filter simpleFilter() {
        return SIMPLE_FILTER;
    }

    // Default constructor is required.
    private Filter() {
        this(0f, 0.5f, Collections.emptySet(), SemanticSynchrony.getConfiguration().getSources().get(0).getName());
    }

    public float getMinWeight() {
        return minWeight;
    }

    /**
     * Get the set of included source names.
     * An empty set means "include all sources".
     */
    public Set<String> getIncludedSources() {
        return includedSources;
    }

    /**
     * @deprecated Use getIncludedSources() instead
     */
    @Deprecated
    public String getMinSource() {
        // For backwards compatibility, return null if using set-based filtering
        return null;
    }

    public void setMinWeight(float minWeight) {
        this.minWeight = minWeight;
    }

    public void setDefaultWeight(float defaultWeight) {
        this.defaultWeight = defaultWeight;
    }

    /**
     * Set the included sources.
     * An empty set means "include all sources".
     */
    public void setIncludedSources(Set<String> includedSources) {
        this.includedSources = includedSources != null ? new HashSet<>(includedSources) : Collections.emptySet();
    }

    /**
     * @deprecated Use setIncludedSources() instead
     */
    @Deprecated
    public void setMinSource(String minSource) {
        // For backwards compatibility, minSource means "show sources at least as public as this"
        // Since sources are ordered from most private (index 0) to most public,
        // minSource="private" (index 0) means "show all sources"
        // We achieve this by setting includedSources to empty set (which means "include all")
        if (minSource == null) {
            this.includedSources = Collections.emptySet();
            return;
        }

        List<DataSource> sources = SemanticSynchrony.getConfiguration().getSources();

        int minIndex = -1;
        for (int i = 0; i < sources.size(); i++) {
            if (sources.get(i).getName().equals(minSource)) {
                minIndex = i;
                break;
            }
        }

        // If minSource is the first (most private) source, include all sources
        // This is equivalent to "no filter on source"
        if (minIndex == 0) {
            this.includedSources = Collections.emptySet();
        } else if (minIndex > 0) {
            Set<String> included = new HashSet<>();
            for (int i = minIndex; i < sources.size(); i++) {
                included.add(sources.get(i).getName());
            }
            this.includedSources = included;
        } else {
            // Source not found, include all
            this.includedSources = Collections.emptySet();
        }
    }

    public void setDefaultSource(String defaultSource) {
        this.defaultSource = defaultSource;
    }

    public String getDefaultSource() {
        return defaultSource;
    }

    public float getDefaultWeight() {
        return defaultWeight;
    }

    public boolean isTrivial() {
        return (includedSources == null || includedSources.isEmpty()) && minWeight == 0;
    }

    public Filter(final float minWeight,
                  final float defaultWeight,
                  final Set<String> includedSources,
                  final String defaultSource) {

        checkBetweenZeroAndOne(minWeight);
        checkBetweenZeroAndOne(defaultWeight);
        Preconditions.checkArgument(defaultWeight >= minWeight, "default weight greater than minimum");

        this.includedSources = includedSources != null ? new HashSet<>(includedSources) : Collections.emptySet();
        this.defaultSource = defaultSource;
        this.minWeight = minWeight;
        this.defaultWeight = defaultWeight;
    }

    /**
     * @deprecated Use constructor with Set<String> includedSources instead
     */
    @Deprecated
    public Filter(final float minWeight,
                  final float defaultWeight,
                  final String minSource,
                  final String defaultSource) {
        this(minWeight, defaultWeight, Collections.emptySet(), defaultSource);
        setMinSource(minSource);  // Convert minSource to includedSources
    }

    private void checkBetweenZeroAndOne(final float value) {
        Preconditions.checkArgument(value >= 0f && value <= 1f, "argument outside of range [0, 1]");
    }

    @Override
    public boolean test(final Note note) {
        String source = Note.getSource(note);
        Float weight = Note.getWeight(note);

        if (null == source || null == weight) return false;

        // Check weight - the weight criterion includes the minimum
        if (weight < minWeight) return false;

        // Check source - empty includedSources means include all
        if (includedSources != null && !includedSources.isEmpty()) {
            if (!includedSources.contains(source)) {
                return false;
            }
        }

        return true;
    }
}
