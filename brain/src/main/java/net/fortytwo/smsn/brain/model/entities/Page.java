package net.fortytwo.smsn.brain.model.entities;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Property;

import java.util.LinkedHashMap;
import java.util.Map;

public interface Page extends Node {

    Map<String, Property> propertiesByKey = createPropertiesByKey();

    static Map<String, Property> createPropertiesByKey() {
        Map<String, Property> propertiesByKey = new LinkedHashMap<>();
        for (Property prop : new Property[]{
                new Property<>(true,true, SemanticSynchrony.PropertyKeys.ALIAS,
                        Page::getAlias, Page::setAlias, s -> s),
                new Property<>(true,false, SemanticSynchrony.PropertyKeys.TEXT,
                        Page::getText, Page::setText, s -> s),
                new Property<>(true,true, SemanticSynchrony.PropertyKeys.SOURCE,
                        Page::getSource, Page::setSource, s -> s),
                new Property<>(true,true, SemanticSynchrony.PropertyKeys.SHORTCUT,
                        Page::getShortcut, Page::setShortcut, s -> s),
                new Property<>(true, true, SemanticSynchrony.PropertyKeys.WEIGHT,
                        Page::getWeight, Page::setWeight, Float::valueOf),
                new Property<>(true, true, SemanticSynchrony.PropertyKeys.PRIORITY,
                        Page::getPriority, Page::setPriority, Float::valueOf),
        }) {
            propertiesByKey.put(prop.getPropertyKey(), prop);
        }
        return propertiesByKey;
    }
    
    String getText();

    void setText(String text);

    String getAlias();

    void setAlias(String alias);

    String getSource();

    void setSource(String source);

    String getShortcut();

    void setShortcut(String shortcut);

    TreeNode<Link> getContent();

    void setContent(TreeNode<Link> tree);

    Float getWeight();

    void setWeight(Float weight);

    Float getPriority();

    void setPriority(Float priority);
}
