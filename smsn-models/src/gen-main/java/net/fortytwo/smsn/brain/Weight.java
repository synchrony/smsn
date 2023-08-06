package net.fortytwo.smsn.brain;

public class Weight {
  public static final hydra.core.Name NAME = new hydra.core.Name("net/fortytwo/smsn/brain.Weight");
  
  public final Float value;
  
  public Weight (Float value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Weight)) {
      return false;
    }
    Weight o = (Weight) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}