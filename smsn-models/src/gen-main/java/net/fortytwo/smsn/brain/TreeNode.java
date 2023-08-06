package net.fortytwo.smsn.brain;

public class TreeNode {
  public static final hydra.core.Name NAME = new hydra.core.Name("net/fortytwo/smsn/brain.TreeNode");
  
  public final net.fortytwo.smsn.brain.AtomId id;
  
  public final net.fortytwo.smsn.brain.Timestamp created;
  
  public final net.fortytwo.smsn.brain.Normed weight;
  
  public final java.util.Optional<net.fortytwo.smsn.brain.Normed> priority;
  
  public final net.fortytwo.smsn.brain.SourceName source;
  
  public final String title;
  
  public final java.util.Optional<String> alias;
  
  public final java.util.List<net.fortytwo.smsn.brain.TreeNode> children;
  
  public final Integer numberOfChildren;
  
  public final Integer numberOfParents;
  
  public TreeNode (net.fortytwo.smsn.brain.AtomId id, net.fortytwo.smsn.brain.Timestamp created, net.fortytwo.smsn.brain.Normed weight, java.util.Optional<net.fortytwo.smsn.brain.Normed> priority, net.fortytwo.smsn.brain.SourceName source, String title, java.util.Optional<String> alias, java.util.List<net.fortytwo.smsn.brain.TreeNode> children, Integer numberOfChildren, Integer numberOfParents) {
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
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
  
  public TreeNode withCreated(net.fortytwo.smsn.brain.Timestamp created) {
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
  
  public TreeNode withWeight(net.fortytwo.smsn.brain.Normed weight) {
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
  
  public TreeNode withPriority(java.util.Optional<net.fortytwo.smsn.brain.Normed> priority) {
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
  
  public TreeNode withSource(net.fortytwo.smsn.brain.SourceName source) {
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
  
  public TreeNode withTitle(String title) {
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
  
  public TreeNode withAlias(java.util.Optional<String> alias) {
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
  
  public TreeNode withChildren(java.util.List<net.fortytwo.smsn.brain.TreeNode> children) {
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
  
  public TreeNode withNumberOfChildren(Integer numberOfChildren) {
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
  
  public TreeNode withNumberOfParents(Integer numberOfParents) {
    return new TreeNode(id, created, weight, priority, source, title, alias, children, numberOfChildren, numberOfParents);
  }
}