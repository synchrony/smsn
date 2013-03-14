package net.fortytwo.extendo.util.properties;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;

/**
 * FIXME: this class is nearly identical to RippleProperties, only slightly
 * better.  It may be worthwhile to let Ripple and MyOtherBrain inherit a
 * common dependency.
 *
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TypedProperties extends Properties {
    private final DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ");

    public TypedProperties(final Properties defaults) {
        super(defaults);
    }

    public TypedProperties() {
        super();
    }

    private String getProperty(final String name,
                               final boolean required) throws PropertyException {
        String s = getProperty(name);

        if (null == s && required) {
            throw new PropertyValueNotFoundException(name);
        }

        return s;
    }

    // String values ///////////////////////////////////////////////////////////

    public String getString(final String name) throws PropertyException {
        return getProperty(name, true);
    }

    public String getString(final String name,
                            final String defaultValue) throws PropertyException {
        String value = getProperty(name, false);

        return (null == value)
                ? defaultValue
                : value;
    }

    public void setString(final String name,
                          final String value) {
        if (null == value) {
            remove(name);
        } else {
            setProperty(name, value);
        }
    }

    // boolean values //////////////////////////////////////////////////////////

    public boolean getBoolean(final String name) throws PropertyException {
        String value = getProperty(name, true);

        return value.equals("true");
    }

    public boolean getBoolean(final String name,
                              final boolean defaultValue) throws PropertyException {
        String value = getProperty(name, false);

        return (null == value)
                ? defaultValue
                : value.equals("true");
    }

    public void setBoolean(final String name,
                           final boolean value) {
        setProperty(name, "" + value);
    }

    // double values ///////////////////////////////////////////////////////////

    public double getDouble(final String name) throws PropertyException {
        String value = getProperty(name, true);

        try {
            return new Double(value);
        }

        catch (NumberFormatException e) {
            throw new InvalidPropertyValueException(name, e);
        }
    }

    public double getDouble(final String name,
                            final double defaultValue) throws PropertyException {
        String value = getProperty(name, false);

        try {
            return (null == value)
                    ? defaultValue
                    : new Double(value);
        }

        catch (NumberFormatException e) {
            throw new InvalidPropertyValueException(name, e);
        }
    }

    public void setDouble(final String name, final double value) {
        setProperty(name, "" + value);
    }

    // float values ////////////////////////////////////////////////////////////

    public float getFloat(final String name) throws PropertyException {
        String value = getProperty(name, true);

        try {
            return new Float(value);
        }

        catch (NumberFormatException e) {
            throw new InvalidPropertyValueException(name, e);
        }
    }

    public float getFloat(final String name, final float defaultValue) throws PropertyException {
        String value = getProperty(name, false);

        try {
            return (null == value)
                    ? defaultValue
                    : new Float(value);
        }

        catch (NumberFormatException e) {
            throw new InvalidPropertyValueException(name, e);
        }
    }

    public void setFloat(final String name, final float value) {
        setProperty(name, "" + value);
    }

    // int values //////////////////////////////////////////////////////////////

    public int getInt(final String name) throws PropertyException {
        String value = getProperty(name, true);

        try {
            return new Integer(value);
        }

        catch (NumberFormatException e) {
            throw new InvalidPropertyValueException(name, e);
        }
    }

    public int getInt(final String name, final int defaultValue) throws PropertyException {
        String value = getProperty(name, false);

        try {
            return (null == value)
                    ? defaultValue
                    : new Integer(value);
        }

        catch (NumberFormatException e) {
            throw new InvalidPropertyValueException(name, e);
        }
    }

    public void setInt(final String name, final int value) {
        setProperty(name, "" + value);
    }

    // long values /////////////////////////////////////////////////////////////

    public long getLong(final String name) throws PropertyException {
        String value = getProperty(name, true);

        try {
            return new Long(value);
        }

        catch (NumberFormatException e) {
            throw new InvalidPropertyValueException(name, e);
        }
    }

    public long getLong(final String name, final long defaultValue) throws PropertyException {
        String value = getProperty(name, false);

        try {
            return (null == value)
                    ? defaultValue
                    : new Long(value);
        }

        catch (NumberFormatException e) {
            throw new InvalidPropertyValueException(name, e);
        }
    }

    public void setLong(final String name, final long value) {
        setProperty(name, "" + value);
    }

    // URI values //////////////////////////////////////////////////////////////

    public URI getURI(final String name) throws PropertyException {
        String value = getProperty(name, true);

        try {
            return new URI(value);
        }

        catch (URISyntaxException e) {
            throw new InvalidPropertyValueException(name, e);
        }
    }

    public URI getURI(final String name, final URI defaultValue) throws PropertyException {
        String value = getProperty(name, false);

        try {
            return (null == value)
                    ? defaultValue
                    : new URI(value);
        }

        catch (URISyntaxException e) {
            throw new InvalidPropertyValueException(name, e);
        }
    }

    public void setURI(final String name, final URI value) {
        if (null == value) {
            remove(name);
        } else {
            setProperty(name, value.toString());
        }
    }

    // URL values //////////////////////////////////////////////////////////////

    public URL getURL(final String name) throws PropertyException {
        String value = getProperty(name, true);

        try {
            return new URL(value);
        }

        catch (MalformedURLException e) {
            throw new InvalidPropertyValueException(name, e);
        }
    }

    public URL getURL(final String name, final URL defaultValue) throws PropertyException {
        String value = getProperty(name, false);

        try {
            return (null == value)
                    ? defaultValue
                    : new URL(value);
        }

        catch (MalformedURLException e) {
            throw new InvalidPropertyValueException(name, e);
        }
    }

    public void setURL(final String name, final URL value) {
        if (null == value) {
            remove(name);
        } else {
            setProperty(name, value.toString());
        }
    }

    // File values ///////////////////////////////////////////////////////////////

    public File getFile(final String name) throws PropertyException {
        String value = getProperty(name, true);

        return new File(value);
    }

    public File getFile(final String name, final File defaultValue) throws PropertyException {
        String value = getProperty(name, false);

        return (null == value)
                ? defaultValue
                : new File(value);
    }

    public void setFile(final String name, final File value) {
        if (null == value) {
            remove(name);
        } else {
            setProperty(name, "" + value.getAbsolutePath());
        }
    }

    // Date values ///////////////////////////////////////////////////////////////

    public Date getDate(final String name) throws PropertyException {
        String value = getProperty(name, true);

        try {
            return dateFormat.parse(value);
        }

        catch (ParseException e) {
            throw new InvalidPropertyValueException(name, e);
        }
    }

    public Date getDate(final String name, final Date defaultValue) throws PropertyException {
        String value = getProperty(name, false);

        try {
            return null == value
                    ? defaultValue
                    : dateFormat.parse(value);
        }

        catch (ParseException e) {
            throw new InvalidPropertyValueException(name, e);
        }
    }

    public void setDate(final String name, final Date value) {
        if (null == value) {
            remove(name);
        } else {
            setProperty(name, "" + dateFormat.format(value));
        }
    }
}
