package net.fortytwo.smsn.brain.model.entities;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Property;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.query.TreeViews;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Function;

public interface Page extends Entity {

    Map<String, Property<Page, ?>> propertiesByKey = createPropertiesByKey();

    static Map<String, Property<Page, ?>> createPropertiesByKey() {
        Map<String, Property<Page, ?>> propertiesByKey = new LinkedHashMap<>();
        for (Property<Page, ?> prop : new Property[]{
                new Property.Builder<Page, Role>()
                        .key(SemanticSynchrony.PropertyKeys.ROLE)
                        .getter(Page::getRole)
                        .setter(Page::setRole)
                        .fromString(Role::valueOf)
                        .defaultValue(Role.Entity)
                        .build(),
                new Property.Builder<Page, String>()
                        .key(SemanticSynchrony.PropertyKeys.ALIAS)
                        .getter(Page::getAlias)
                        .setter(Page::setAlias)
                        .fromString(s -> s)
                        .build(),
                new Property.Builder<Page, Long>()
                        .key(SemanticSynchrony.PropertyKeys.CREATED)
                        .isSettable(false)
                        .getter(Page::getCreated)
                        .setter(Page::setCreated)
                        .fromString(Long::valueOf)
                        .build(),
                new Property.Builder<Page, String>()
                        .key(SemanticSynchrony.PropertyKeys.TEXT)
                        //.isAnnotationProperty(false)
                        .getter(Page::getText)
                        .setter(Page::setText)
                        .fromString(s -> s)
                        .build(),
                new Property.Builder<Page, String>()
                        .key(SemanticSynchrony.PropertyKeys.SOURCE)
                        .getter(Page::getSource)
                        .setter(Page::setSource)
                        .fromString(s -> s)
                        .build(),
                new Property.Builder<Page, String>()
                        .key(SemanticSynchrony.PropertyKeys.SHORTCUT)
                        .getter(Page::getShortcut)
                        .setter(Page::setShortcut)
                        .fromString(s -> s)
                        .build(),
                new Property.Builder<Page, Float>()
                        .key(SemanticSynchrony.PropertyKeys.WEIGHT)
                        .getter(Page::getWeight)
                        .setter(Page::setWeight)
                        .fromString(Float::valueOf)
                        .defaultValue(0.5f)
                        .build(),
                new Property.Builder<Page, Float>()
                        .key(SemanticSynchrony.PropertyKeys.PRIORITY)
                        .getter(Page::getPriority)
                        .setter(Page::setPriority)
                        .fromString(Float::valueOf)
                        .defaultValue(0f)
                        .build()
        }) {
            propertiesByKey.put(prop.getKey(), prop);
        }
        return propertiesByKey;
    }

    Role getRole();

    void setRole(Role role);

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

    Long getCreated();

    void setCreated(Long created);
}
