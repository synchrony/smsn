package net.fortytwo.extendo.brain.rdf;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Field {
    private final boolean isUnique;
    private final Pattern valueRegex;
    private final BottomUpType dataType;
    private final BottomUpType containedDataType;
    private final Mapper mapper;

    /**
     * @param isUnique whether the presence of this field uniquely identifies the note as an instance of the instantiating class
     * @param valueRegex a regular expression which the value of this field must match
     * @param dataType the data type of this field
     * @param containedDataType if this field is of a container type, the contained type
     * @param mapper a mapping from the note and field value to RDF statements
     */
    public Field(final boolean isUnique,
                 final Pattern valueRegex,
                 final BottomUpType dataType,
                 final BottomUpType containedDataType,
                 final Mapper mapper) {
        this.isUnique = isUnique;
        this.valueRegex = valueRegex;
        this.dataType = dataType;
        this.containedDataType = containedDataType;
        this.mapper = mapper;
    }

    public boolean getIsUnique() {
        return isUnique;
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
