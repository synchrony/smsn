// Note: this is an automatically generated file. Do not edit.

package net.fortytwo.smsn.brain;

import java.io.Serializable;

public class SourceName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("net/fortytwo/smsn/brain.SourceName");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public SourceName (String value) {
    java.util.Objects.requireNonNull((value));
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