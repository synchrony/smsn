package net.fortytwo.smsn.brain;

public class Normed {
  public static final hydra.core.Name NAME = new hydra.core.Name("net/fortytwo/smsn/brain.Normed");
  
  public final Float value;
  
  public Normed (Float value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Normed)) {
      return false;
    }
    Normed o = (Normed) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}