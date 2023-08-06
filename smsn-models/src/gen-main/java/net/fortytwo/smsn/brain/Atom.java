package net.fortytwo.smsn.brain;

public class Atom {
  public static final hydra.core.Name NAME = new hydra.core.Name("net/fortytwo/smsn/brain.Atom");
  
  public final net.fortytwo.smsn.brain.AtomId id;
  
  public final net.fortytwo.smsn.brain.Timestamp created;
  
  public final net.fortytwo.smsn.brain.Weight weight;
  
  public final java.util.Optional<net.fortytwo.smsn.brain.Priority> priority;
  
  public final net.fortytwo.smsn.brain.SourceName source;
  
  public final String title;
  
  public final java.util.Optional<String> alias;
  
  public final java.util.List<net.fortytwo.smsn.brain.AtomId> children;
  
  public Atom (net.fortytwo.smsn.brain.AtomId id, net.fortytwo.smsn.brain.Timestamp created, net.fortytwo.smsn.brain.Weight weight, java.util.Optional<net.fortytwo.smsn.brain.Priority> priority, net.fortytwo.smsn.brain.SourceName source, String title, java.util.Optional<String> alias, java.util.List<net.fortytwo.smsn.brain.AtomId> children) {
    this.id = id;
    this.created = created;
    this.weight = weight;
    this.priority = priority;
    this.source = source;
    this.title = title;
    this.alias = alias;
    this.children = children;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Atom)) {
      return false;
    }
    Atom o = (Atom) (other);
    return id.equals(o.id) && created.equals(o.created) && weight.equals(o.weight) && priority.equals(o.priority) && source.equals(o.source) && title.equals(o.title) && alias.equals(o.alias) && children.equals(o.children);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * created.hashCode() + 5 * weight.hashCode() + 7 * priority.hashCode() + 11 * source.hashCode() + 13 * title.hashCode() + 17 * alias.hashCode() + 19 * children.hashCode();
  }
  
  public Atom withId(net.fortytwo.smsn.brain.AtomId id) {
    return new Atom(id, created, weight, priority, source, title, alias, children);
  }
  
  public Atom withCreated(net.fortytwo.smsn.brain.Timestamp created) {
    return new Atom(id, created, weight, priority, source, title, alias, children);
  }
  
  public Atom withWeight(net.fortytwo.smsn.brain.Weight weight) {
    return new Atom(id, created, weight, priority, source, title, alias, children);
  }
  
  public Atom withPriority(java.util.Optional<net.fortytwo.smsn.brain.Priority> priority) {
    return new Atom(id, created, weight, priority, source, title, alias, children);
  }
  
  public Atom withSource(net.fortytwo.smsn.brain.SourceName source) {
    return new Atom(id, created, weight, priority, source, title, alias, children);
  }
  
  public Atom withTitle(String title) {
    return new Atom(id, created, weight, priority, source, title, alias, children);
  }
  
  public Atom withAlias(java.util.Optional<String> alias) {
    return new Atom(id, created, weight, priority, source, title, alias, children);
  }
  
  public Atom withChildren(java.util.List<net.fortytwo.smsn.brain.AtomId> children) {
    return new Atom(id, created, weight, priority, source, title, alias, children);
  }
}