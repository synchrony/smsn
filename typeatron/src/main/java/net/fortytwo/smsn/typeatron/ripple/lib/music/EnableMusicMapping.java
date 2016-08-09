package net.fortytwo.smsn.typeatron.ripple.lib.music;

import net.fortytwo.smsn.typeatron.ripple.lib.SmSnLibrary;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.PrimitiveStackMapping;
import net.fortytwo.ripple.model.RippleList;

public class EnableMusicMapping extends PrimitiveStackMapping {

    private final TypeatronMusicControl music;

    public EnableMusicMapping(final TypeatronMusicControl music) {
        this.music = music;
    }

    public String[] getIdentifiers() {
        return new String[]{
                SmSnLibrary.NS_2014_12 + "enable-music"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{};
    }

    public String getComment() {
        return "enables musical output from the Typeatron";
    }

    public void apply(final RippleList stack,
                      final Sink<RippleList> solutions,
                      final ModelConnection context) throws RippleException {

        music.enable();

        solutions.accept(stack);
    }
}
