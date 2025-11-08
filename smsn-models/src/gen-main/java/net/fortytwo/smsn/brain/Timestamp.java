// Note: this is an automatically generated file. Do not edit.

package net.fortytwo.smsn.brain;

import java.io.Serializable;

/**
 * A Unix timestamp in milliseconds
 */
public class Timestamp implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("net/fortytwo/smsn/brain.Timestamp");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final Long value;

  public Timestamp (Long value) {
    java.util.Objects.requireNonNull((value));
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
