// Note: this is an automatically generated file. Do not edit.

package net.fortytwo.smsn.brain;

import java.io.Serializable;

public class TreeNode implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("net/fortytwo/smsn/brain.TreeNode");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_CREATED = new hydra.core.Name("created");
  
  public static final hydra.core.Name FIELD_NAME_WEIGHT = new hydra.core.Name("weight");
  
  public static final hydra.core.Name FIELD_NAME_PRIORITY = new hydra.core.Name("priority");
  
  public static final hydra.core.Name FIELD_NAME_SOURCE = new hydra.core.Name("source");
  
  public static final hydra.core.Name FIELD_NAME_TITLE = new hydra.core.Name("title");
  
  public static final hydra.core.Name FIELD_NAME_ALIAS = new hydra.core.Name("alias");
  
  public static final hydra.core.Name FIELD_NAME_CHILDREN = new hydra.core.Name("children");
  
  public static final hydra.core.Name FIELD_NAME_NUMBER_OF_CHILDREN = new hydra.core.Name("numberOfChildren");
  
  public static final hydra.core.Name FIELD_NAME_NUMBER_OF_PARENTS = new hydra.core.Name("numberOfParents");
  
  public final net.fortytwo.smsn.brain.AtomId id;
  
  public final net.fortytwo.smsn.brain.Timestamp created;
  
  public final net.fortytwo.smsn.brain.Normed weight;
  
  public final hydra.util.Opt<net.fortytwo.smsn.brain.Normed> priority;
  
  public final net.fortytwo.smsn.brain.SourceName source;
  
  public final String title;
  
  public final hydra.util.Opt<String> alias;
  
  public final java.util.List<net.fortytwo.smsn.brain.TreeNode> children;
  
  public final Integer numberOfChildren;
  
  public final Integer numberOfParents;
  
  public TreeNode (net.fortytwo.smsn.brain.AtomId id, net.fortytwo.smsn.brain.Timestamp created, net.fortytwo.smsn.brain.Normed weight, hydra.util.Opt<net.fortytwo.smsn.brain.Normed> priority, net.fortytwo.smsn.brain.SourceName source, String title, hydra.util.Opt<String> alias, java.util.List<net.fortytwo.smsn.brain.TreeNode> children, Integer numberOfChildren, Integer numberOfParents) {
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((created));
    java.util.Objects.requireNonNull((weight));
    java.util.Objects.requireNonNull((priority));
    java.util.Objects.requireNonNull((source));
    java.util.Objects.requireNonNull((title));
    java.util.Objects.requireNonNull((alias));
    java.util.Objects.requireNonNull((children));
    java.util.Objects.requireNonNull((numberOfChildren));
    java.util.Objects.requireNonNull((numberOfParents));
    this.id = id;
    this.created = created;
    this.weight = weight;
    this.priority = priority;
    this.source = source;
    this.title = title;
    this.alias = alias;
    this.children = children;
    this.numberOfChildren = numberOfChildren;
    this.numberOfParents = numberOfParents;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TreeNode)) {
      return false;
    }
    TreeNode o = (TreeNode) (other);
    return id.equals(o.id) && created.equals(o.created) && weight.equals(o.weight) && priority.equals(o.priority) && source.equals(o.source) && title.equals(o.title) && alias.equals(o.alias) && children.equals(o.children) && numberOfChildren.equals(o.numberOfChildren) && numberOfParents.equals(o.numberOfParents);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * created.hashCode() + 5 * weight.hashCode() + 7 * priority.hashCode() + 11 * source.hashCode() + 13 * title.hashCode() + 17 * alias.hashCode() + 19 * children.hashCode() + 23 * numberOfChildren.hashCode() + 29 * numberOfParents.hashCode();
  }
  
  public TreeNode withId(net.fortytwo.smsn.brain.AtomId id) {
    java.util.Objects.requireNonNull((id));
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
  
  public TreeNode withCreated(net.fortytwo.smsn.brain.Timestamp created) {
    java.util.Objects.requireNonNull((created));
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
  
  public TreeNode withWeight(net.fortytwo.smsn.brain.Normed weight) {
    java.util.Objects.requireNonNull((weight));
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
  
  public TreeNode withPriority(hydra.util.Opt<net.fortytwo.smsn.brain.Normed> priority) {
    java.util.Objects.requireNonNull((priority));
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
  
  public TreeNode withSource(net.fortytwo.smsn.brain.SourceName source) {
    java.util.Objects.requireNonNull((source));
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
  
  public TreeNode withTitle(String title) {
    java.util.Objects.requireNonNull((title));
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
  
  public TreeNode withAlias(hydra.util.Opt<String> alias) {
    java.util.Objects.requireNonNull((alias));
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
  
  public TreeNode withChildren(java.util.List<net.fortytwo.smsn.brain.TreeNode> children) {
    java.util.Objects.requireNonNull((children));
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
  
  public TreeNode withNumberOfChildren(Integer numberOfChildren) {
    java.util.Objects.requireNonNull((numberOfChildren));
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
  
  public TreeNode withNumberOfParents(Integer numberOfParents) {
    java.util.Objects.requireNonNull((numberOfParents));
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
}