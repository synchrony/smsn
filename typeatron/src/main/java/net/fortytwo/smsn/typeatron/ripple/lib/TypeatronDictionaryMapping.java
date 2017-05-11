package net.fortytwo.smsn.typeatron.ripple.lib;

import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.p2p.SideEffects;
import net.fortytwo.smsn.typeatron.TypeatronControl;
import net.fortytwo.smsn.typeatron.ripple.BrainClient;
import net.fortytwo.smsn.typeatron.ripple.lib.music.DisableMusicMapping;
import net.fortytwo.smsn.typeatron.ripple.lib.music.EnableMusicMapping;
import net.fortytwo.flow.Sink;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.libs.control.Apply;
import net.fortytwo.ripple.libs.control.Branch;
import net.fortytwo.ripple.libs.control.Choice;
import net.fortytwo.ripple.libs.control.Dip;
import net.fortytwo.ripple.libs.control.Dipd;
import net.fortytwo.ripple.libs.control.Fold;
import net.fortytwo.ripple.libs.control.Ifte;
import net.fortytwo.ripple.libs.control.Inverse;
import net.fortytwo.ripple.libs.control.OptionApply;
import net.fortytwo.ripple.libs.control.PlusApply;
import net.fortytwo.ripple.libs.control.RangeApply;
import net.fortytwo.ripple.libs.control.Require;
import net.fortytwo.ripple.libs.control.StarApply;
import net.fortytwo.ripple.libs.control.TimesApply;
import net.fortytwo.ripple.libs.control.While;
import net.fortytwo.ripple.libs.data.Compare;
import net.fortytwo.ripple.libs.data.Equal;
import net.fortytwo.ripple.libs.data.Gt;
import net.fortytwo.ripple.libs.data.Gte;
import net.fortytwo.ripple.libs.data.Lang;
import net.fortytwo.ripple.libs.data.Lt;
import net.fortytwo.ripple.libs.data.Lte;
import net.fortytwo.ripple.libs.data.ToDouble;
import net.fortytwo.ripple.libs.data.ToInteger;
import net.fortytwo.ripple.libs.data.ToMillis;
import net.fortytwo.ripple.libs.data.ToString;
import net.fortytwo.ripple.libs.data.ToUri;
import net.fortytwo.ripple.libs.logic.And;
import net.fortytwo.ripple.libs.logic.Not;
import net.fortytwo.ripple.libs.logic.Or;
import net.fortytwo.ripple.libs.logic.Xor;
import net.fortytwo.ripple.libs.math.Abs;
import net.fortytwo.ripple.libs.math.Acos;
import net.fortytwo.ripple.libs.math.Add;
import net.fortytwo.ripple.libs.math.Asin;
import net.fortytwo.ripple.libs.math.Atan;
import net.fortytwo.ripple.libs.math.Atan2;
import net.fortytwo.ripple.libs.math.Cbrt;
import net.fortytwo.ripple.libs.math.Ceil;
import net.fortytwo.ripple.libs.math.Cos;
import net.fortytwo.ripple.libs.math.Cosh;
import net.fortytwo.ripple.libs.math.Div;
import net.fortytwo.ripple.libs.math.Exp;
import net.fortytwo.ripple.libs.math.Floor;
import net.fortytwo.ripple.libs.math.Log;
import net.fortytwo.ripple.libs.math.Log10;
import net.fortytwo.ripple.libs.math.Mod;
import net.fortytwo.ripple.libs.math.Mul;
import net.fortytwo.ripple.libs.math.Neg;
import net.fortytwo.ripple.libs.math.Pow;
import net.fortytwo.ripple.libs.math.Sign;
import net.fortytwo.ripple.libs.math.Sin;
import net.fortytwo.ripple.libs.math.Sinh;
import net.fortytwo.ripple.libs.math.Sqrt;
import net.fortytwo.ripple.libs.math.Sub;
import net.fortytwo.ripple.libs.math.Tan;
import net.fortytwo.ripple.libs.math.Tanh;
import net.fortytwo.ripple.libs.stack.At;
import net.fortytwo.ripple.libs.stack.Cat;
import net.fortytwo.ripple.libs.stack.Cons;
import net.fortytwo.ripple.libs.stack.Drop;
import net.fortytwo.ripple.libs.stack.Dup;
import net.fortytwo.ripple.libs.stack.Dupd;
import net.fortytwo.ripple.libs.stack.Empty;
import net.fortytwo.ripple.libs.stack.Has;
import net.fortytwo.ripple.libs.stack.In;
import net.fortytwo.ripple.libs.stack.Max;
import net.fortytwo.ripple.libs.stack.Min;
import net.fortytwo.ripple.libs.stack.Of;
import net.fortytwo.ripple.libs.stack.Pop;
import net.fortytwo.ripple.libs.stack.Popd;
import net.fortytwo.ripple.libs.stack.Rolldown;
import net.fortytwo.ripple.libs.stack.Rolldownd;
import net.fortytwo.ripple.libs.stack.Rollup;
import net.fortytwo.ripple.libs.stack.Rollupd;
import net.fortytwo.ripple.libs.stack.Rotate;
import net.fortytwo.ripple.libs.stack.Rotated;
import net.fortytwo.ripple.libs.stack.Self;
import net.fortytwo.ripple.libs.stack.Size;
import net.fortytwo.ripple.libs.stack.Swap;
import net.fortytwo.ripple.libs.stack.Swapd;
import net.fortytwo.ripple.libs.stack.Swons;
import net.fortytwo.ripple.libs.stack.Take;
import net.fortytwo.ripple.libs.stack.Top;
import net.fortytwo.ripple.libs.stack.Uncons;
import net.fortytwo.ripple.libs.stack.Unswons;
import net.fortytwo.ripple.libs.stream.Both;
import net.fortytwo.ripple.libs.stream.Count;
import net.fortytwo.ripple.libs.stream.Distinct;
import net.fortytwo.ripple.libs.stream.Each;
import net.fortytwo.ripple.libs.stream.Intersect;
import net.fortytwo.ripple.libs.stream.Limit;
import net.fortytwo.ripple.libs.stream.Order;
import net.fortytwo.ripple.libs.stream.Scrap;
import net.fortytwo.ripple.libs.string.Concat;
import net.fortytwo.ripple.libs.string.Contains;
import net.fortytwo.ripple.libs.string.EndsWith;
import net.fortytwo.ripple.libs.string.IndexOf;
import net.fortytwo.ripple.libs.string.LastIndexOf;
import net.fortytwo.ripple.libs.string.Length;
import net.fortytwo.ripple.libs.string.Matches;
import net.fortytwo.ripple.libs.string.Md5;
import net.fortytwo.ripple.libs.string.PercentDecoded;
import net.fortytwo.ripple.libs.string.PercentEncoded;
import net.fortytwo.ripple.libs.string.ReplaceAll;
import net.fortytwo.ripple.libs.string.Sha1;
import net.fortytwo.ripple.libs.string.Split;
import net.fortytwo.ripple.libs.string.StartsWith;
import net.fortytwo.ripple.libs.string.Substring;
import net.fortytwo.ripple.libs.string.ToLowerCase;
import net.fortytwo.ripple.libs.string.ToUpperCase;
import net.fortytwo.ripple.libs.string.Trim;
import net.fortytwo.ripple.libs.system.Get;
import net.fortytwo.ripple.libs.system.Random;
import net.fortytwo.ripple.libs.system.Script;
import net.fortytwo.ripple.libs.system.System;
import net.fortytwo.ripple.libs.system.Time;
import net.fortytwo.ripple.model.ModelConnection;
import net.fortytwo.ripple.model.Operator;
import net.fortytwo.ripple.model.PrimitiveStackMapping;
import net.fortytwo.ripple.model.RippleList;
import net.fortytwo.ripple.model.StackMapping;

import java.util.HashMap;
import java.util.Map;

public class TypeatronDictionaryMapping extends PrimitiveStackMapping {

    private final Map<String, StackMapping> dictionary = new HashMap<>();

    public TypeatronDictionaryMapping(final SideEffects environment,
                                      final TypeatronControl typeatron) throws RippleException {
        BrainClient exoBrainClient;
        try {
            exoBrainClient = new BrainClient();
        } catch (BrainClient.BrainClientException e) {
            throw new RippleException(e);
        }

        Filter defaultFilter = Filter.noFilter();
        // brain stream atoms begin with low weight, and with private rather than personal sharability
        // TODO: don't hard-code the source
        Filter brainstreamFilter = new Filter(0f, 0.25f, 0f, "private");

        // environment-dependent library
        add(new SpeakMapping(environment), "s", "speak");

        // Typeatron hardware library
        add(new LaserPointerMapping(exoBrainClient, defaultFilter, typeatron), "l", "laser");
        add(new GetLightLevelMapping(typeatron), "ll", "light");
        add(new MorseMapping(typeatron), "m", "morse");
        add(new VibrateMapping(typeatron), "V", "vibrate");

        // Typeatron internals
        // TODO: merge these with shortcut get/set and search
        //add(new DictionaryPutMapping(userDictionary), "i");
        //add(new DictionaryGetMapping(userDictionary), "o");

        // Extend-o-Brain
        add(new GetAtomAliasMapping(exoBrainClient, defaultFilter), "@a");
        add(new GetAtomChildrenMapping(exoBrainClient, defaultFilter), "@n");
        add(new GetAtomCreatedMapping(exoBrainClient, defaultFilter), "@c");
        add(new GetAtomIdMapping(exoBrainClient, defaultFilter), "@i");
        add(new GetAtomSourceMapping(exoBrainClient, defaultFilter), "@y");
        add(new GetAtomShortcutMapping(exoBrainClient, defaultFilter), "@t");
        add(new GetAtomValueMapping(exoBrainClient, defaultFilter), "@v", "v");
        add(new GetAtomWeightMapping(exoBrainClient, defaultFilter), "@w");
        add(new SetAtomAliasMapping(exoBrainClient, defaultFilter), "a@");
        add(new SetAtomSourceMapping(exoBrainClient, defaultFilter), "y@");
        add(new SetAtomShortcutMapping(exoBrainClient, defaultFilter), "t@", "sh");  // TODO: mismatch/redundancy
        add(new SetAtomValueMapping(exoBrainClient, defaultFilter), "v@");
        add(new SetAtomWeightMapping(exoBrainClient, defaultFilter), "w@");
        add(new ShortcutSearchMapping(exoBrainClient, defaultFilter), "o", "shortcut-search");
        add(new AddToStreamMapping(exoBrainClient, brainstreamFilter), "a", "append");
        // TODO: temporary/change
        add(new GiveAtomMapping(exoBrainClient, defaultFilter, typeatron), "g", "give");

        // Typeatron music
        add(new EnableMusicMapping(typeatron.getMusic()), "me");
        add(new DisableMusicMapping(typeatron.getMusic()), "md");

        // control library
        add(new Apply(), ".");
        add(new Branch());
        add(new Choice());
        add(new Dip());
        add(new Dipd());
        add(new Fold());
        add(new Ifte());
        add(new Inverse(), "~");
        add(new net.fortytwo.ripple.libs.control.Map());
        add(new OptionApply(), "?");
        add(new PlusApply()); // note: '+' would conflict with 'add' alias
        add(new RangeApply());
        add(new Require());
        add(new StarApply()); // note: '*' would conflict with 'mul' alias
        add(new TimesApply());
        add(new While());

        // data library
        add(new Compare());
        add(new Equal(), "=");
        add(new Gt(), ">");
        add(new Gte(), ">=");
        add(new Lang());
        add(new Lt(), "<");
        add(new Lte(), "<=");
        add(new ToDouble());
        add(new ToInteger());
        add(new ToMillis());
        add(new ToString());
        add(new ToUri());
        add(new net.fortytwo.ripple.libs.data.Type());

        // note: graph library primitives are currently not included

        // logic library
        add(new And());
        add(new Not());
        add(new Or());
        add(new Xor());

        // math library
        add(new Abs());
        add(new Acos());
        add(new Add(), "+");
        add(new Asin());
        add(new Atan());
        add(new Atan2());
        add(new Cbrt());
        add(new Ceil());
        add(new Cos());
        add(new Cosh());
        add(new Div(), "/");
        add(new Exp());
        add(new Floor());
        add(new Log());
        add(new Log10());
        add(new Mod(), "%");
        add(new Mul(), "*");
        add(new Neg());
        add(new Pow(), "^");
        add(new Sign());
        add(new Sin());
        add(new Sinh());
        add(new Sqrt());
        add(new Sub(), "-");
        add(new Tan());
        add(new Tanh());

        // stack library
        add(new At());
        add(new Cat());
        add(new Cons());
        add(new Drop());
        add(new Dup(), "d");
        add(new Dupd());
        add(new Empty());
        add(new Has());
        add(new In());
        add(new Max());
        add(new Min());
        add(new Of());
        add(new Pop(), "p");
        add(new Popd(), "pd");
        add(new Rolldown(), "rd");
        add(new Rolldownd());
        add(new Rollup(), "ru");
        add(new Rollupd());
        add(new Rotate(), "rt");
        add(new Rotated());
        add(new Self());
        add(new Size(), "z");
        add(new Swap(), "w");
        add(new Swapd());
        add(new Swons());
        add(new Take());
        add(new Top());
        add(new Uncons());
        add(new Unswons());

        // stream library
        add(new Both(), "eb");
        add(new Count(), "ec");
        add(new Distinct(), "ed");
        add(new Each(), "ee");
        add(new Intersect(), "ei");
        add(new Limit(), "el");
        add(new Order(), "eo");
        add(new Scrap(), "es");

        // string library
        add(new Concat(), "icc", "c");
        add(new Contains(), "ict");
        add(new EndsWith(), "iew");
        add(new IndexOf(), "iio");
        add(new LastIndexOf(), "ilio");
        add(new Length(), "il");
        add(new Matches(), "im");
        add(new Md5(), "imd");
        add(new PercentDecoded(), "ipd");
        add(new PercentEncoded(), "ipe");
        add(new ReplaceAll(), "ira");
        add(new Sha1(), "ish");
        add(new Split(), "is");
        add(new StartsWith(), "isw");
        add(new Substring(), "iss");
        add(new ToLowerCase(), "itlc");
        add(new ToUpperCase(), "ituc");
        add(new Trim(), "it");

        // system library
        add(new Get());
        add(new Random(), "r");
        add(new Script());
        add(new System());
        add(new Time(), "t");
    }

    private void add(final StackMapping mapping,
                     final String... aliases) {
        for (String name : aliases) {
            add(name, mapping);
        }

        if (mapping instanceof PrimitiveStackMapping) {
            add(((PrimitiveStackMapping) mapping).getIdentifiers()[0], mapping);
        }
    }

    private void add(final String alias,
                     final StackMapping cv) {
        if (null != dictionary.get(alias)) {
            throw new IllegalStateException("duplicate control alias '" + alias + "' for mapping " + cv
                    + " (previously used for " + dictionary.get(alias) + ")");
        }

        dictionary.put(alias, cv);
    }

    public String[] getIdentifiers() {
        return new String[]{
                SmSnLibrary.NS_2014_12 + "tt-op"
        };
    }

    public Parameter[] getParameters() {
        return new Parameter[]{new Parameter("name", "the name of the Ripple primitive to activate", true)};
    }

    public String getComment() {
        return "a specialized variant of Ripple's 'op' operator." +
                " It activates any of a Typeatron-specific library of primitives" +
                " based on a set of rapid-input aliases";
    }

    public void apply(final RippleList arg,
                      final Sink<RippleList> solutions,
                      final ModelConnection context) throws RippleException {
        String name = context.toString(arg.getFirst());

        StackMapping v = dictionary.get(name);
        if (null != v) {
            // TODO: just keep a dictionary of operators, rather than a dictionary of mappings
            Operator op = new Operator(v);

            solutions.accept(arg.getRest().push(op));
        }
    }
}
