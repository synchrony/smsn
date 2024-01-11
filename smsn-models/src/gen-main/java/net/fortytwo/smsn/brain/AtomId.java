package net.fortytwo.smsn.brain;

public class AtomId {
  public static final hydra.core.Name NAME = new hydra.core.Name("net/fortytwo/smsn/brain.AtomId");
  
  public final String value;
  
  public AtomId (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AtomId)) {
      return false;
    }
    AtomId o = (AtomId) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}