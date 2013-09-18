package net.fortytwo.extendo.brain.rdf;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Field {
    private final String valueRegex;
    private final BottomUpType dataType;
    private final BottomUpType containedDataType;
    private final Mapper mapper;

    public Field(final String valueRegex,
                 final BottomUpType dataType,
                 final BottomUpType containedDataType,
                 final Mapper mapper) {
        this.valueRegex = valueRegex;
        this.dataType = dataType;
        this.containedDataType = containedDataType;
        this.mapper = mapper;
    }

    public String getValueRegex() {
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
