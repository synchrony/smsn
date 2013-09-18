package net.fortytwo.extendo.brain.rdf;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Field {
    private final Pattern valueRegex;
    private final BottomUpType dataType;
    private final BottomUpType containedDataType;
    private final Mapper mapper;

    public Field(final Pattern valueRegex,
                 final BottomUpType dataType,
                 final BottomUpType containedDataType,
                 final Mapper mapper) {
        this.valueRegex = valueRegex;
        this.dataType = dataType;
        this.containedDataType = containedDataType;
        this.mapper = mapper;
    }

    public Pattern getValueRegex() {
        return valueRegex;
    }

    public BottomUpType getDataType() {
        return dataType;
    }

    public BottomUpType getContainedDataType() {
        return containedDataType;
    }

    public Mapper getMapper() {
        return mapper;
    }
}
