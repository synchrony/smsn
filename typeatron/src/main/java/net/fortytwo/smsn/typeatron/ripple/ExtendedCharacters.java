package net.fortytwo.smsn.typeatron.ripple;

import java.util.HashMap;
import java.util.Map;

/**
 * A mapping of vowels and consonants to their modified counterparts through a small set of diacritics.
 * This mapping covers all single-vowel and consonant symbols from Unicode 00c0 through 00ff,
 * excluding the AE and ae ligatures.
 * The pairing of consonants with diacritics is somewhat ambiguous, so choices have been made for mnemonic
 * value and ease of chording with the Typeatron.
 * For example, the enye symbol in "sen~or" is obviously an n modified by a tilde,
 * but what is the modifier in c-cedilla?  Here, it is considered to be an acute.
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ExtendedCharacters {

    public enum Diacritic {
        Grave, Acute, Circumflex, Tilde, Dieresis, Overring, Slash
    }

    private final Map<Diacritic, Map<Character, Character>> diacriticMaps;

    ExtendedCharacters() {
        diacriticMaps = new HashMap<Diacritic, Map<Character, Character>>();
        for (Diacritic d : Diacritic.values()) {
            diacriticMaps.put(d, new HashMap<Character, Character>());
        }

        add(Diacritic.Grave, 'A', '\u00c0');
        add(Diacritic.Acute, 'A', '\u00c1');
        add(Diacritic.Circumflex, 'A', '\u00c2');
        add(Diacritic.Tilde, 'A', '\u00c3');
        add(Diacritic.Dieresis, 'A', '\u00c4');
        add(Diacritic.Overring, 'A', '\u00c5');
        add(Diacritic.Grave, 'a', '\u00e0');
        add(Diacritic.Acute, 'a', '\u00e1');
        add(Diacritic.Circumflex, 'a', '\u00e2');
        add(Diacritic.Tilde, 'a', '\u00e3');
        add(Diacritic.Dieresis, 'a', '\u00e4');
        add(Diacritic.Overring, 'a', '\u00e5');
        add(Diacritic.Grave, 'E', '\u00c8');
        add(Diacritic.Acute, 'E', '\u00c9');
        add(Diacritic.Circumflex, 'E', '\u00ca');
        add(Diacritic.Dieresis, 'E', '\u00cb');
        add(Diacritic.Grave, 'e', '\u00e8');
        add(Diacritic.Acute, 'e', '\u00e9');
        add(Diacritic.Circumflex, 'e', '\u00ea');
        add(Diacritic.Dieresis, 'e', '\u00eb');
        add(Diacritic.Grave, 'I', '\u00cc');
        add(Diacritic.Acute, 'I', '\u00cd');
        add(Diacritic.Circumflex, 'I', '\u00ce');
        add(Diacritic.Dieresis, 'I', '\u00cf');
        add(Diacritic.Grave, 'i', '\u00ec');
        add(Diacritic.Acute, 'i', '\u00ed');
        add(Diacritic.Circumflex, 'i', '\u00ee');
        add(Diacritic.Dieresis, 'i', '\u00ef');
        add(Diacritic.Grave, 'O', '\u00d2');
        add(Diacritic.Acute, 'O', '\u00d3');
        add(Diacritic.Circumflex, 'O', '\u00d4');
        add(Diacritic.Tilde, 'O', '\u00d5');
        add(Diacritic.Dieresis, 'O', '\u00d6');
        add(Diacritic.Slash, 'O', '\u00d8');
        add(Diacritic.Grave, 'o', '\u00f2');
        add(Diacritic.Acute, 'o', '\u00f3');
        add(Diacritic.Circumflex, 'o', '\u00f4');
        add(Diacritic.Tilde, 'o', '\u00f5');
        add(Diacritic.Dieresis, 'o', '\u00f6');
        add(Diacritic.Slash, 'o', '\u00f8');
        add(Diacritic.Grave, 'U', '\u00d9');
        add(Diacritic.Acute, 'U', '\u00da');
        add(Diacritic.Circumflex, 'U', '\u00db');
        add(Diacritic.Dieresis, 'U', '\u00dc');
        add(Diacritic.Grave, 'u', '\u00f9');
        add(Diacritic.Acute, 'u', '\u00fa');
        add(Diacritic.Circumflex, 'u', '\u00fb');
        add(Diacritic.Dieresis, 'u', '\u00fc');
        add(Diacritic.Acute, 'C', '\u00c7');  // C cedilla
        add(Diacritic.Acute, 'c', '\u00e7');  // c cedilla
        add(Diacritic.Slash, 'D', '\u00d0');  // uppercase eth
        add(Diacritic.Slash, 'd', '\u00f0');  // lowercase eth
        add(Diacritic.Tilde, 'N', '\u00d1');  // N tilde
        add(Diacritic.Tilde, 'n', '\u00f1');  // n tilde
        add(Diacritic.Slash, 'T', '\u00de');  // uppercase thorn
        add(Diacritic.Slash, 't', '\u00fe');  // lowercase thorn
        add(Diacritic.Acute, 'Y', '\u00dd');  // Y acute
        add(Diacritic.Acute, 'y', '\u00fd');  // y acute
        add(Diacritic.Dieresis, 'y', '\u00ff');  // y dieresis
    }

    private void add(Diacritic d, char from, char to) {
        Map<Character, Character> map = diacriticMaps.get(d);
        map.put(from, to);
    }

    public Character modify(final Diacritic d,
                            final char c) {
        return diacriticMaps.get(d).get(c);
    }
}
