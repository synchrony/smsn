package net.fortytwo.smsn.brain;

/**
 * A Unix timestamp in seconds
 */
public class Timestamp {
  public static final hydra.core.Name NAME = new hydra.core.Name("net/fortytwo/smsn/brain.Timestamp");
  
  /**
   * A Unix timestamp in seconds
   */
  public final Integer value;
  
  public Timestamp (Integer value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Timestamp)) {
      return false;
    }
    Timestamp o = (Timestamp) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}