package net.fortytwo.smsn.brain;

public class SourceName {
  public static final hydra.core.Name NAME = new hydra.core.Name("net/fortytwo/smsn/brain.SourceName");
  
  public final String value;
  
  public SourceName (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SourceName)) {
      return false;
    }
    SourceName o = (SourceName) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}