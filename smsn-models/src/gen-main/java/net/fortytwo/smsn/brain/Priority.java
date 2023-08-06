package net.fortytwo.smsn.brain;

public class Priority {
  public static final hydra.core.Name NAME = new hydra.core.Name("net/fortytwo/smsn/brain.Priority");
  
  public final Float value;
  
  public Priority (Float value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Priority)) {
      return false;
    }
    Priority o = (Priority) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}