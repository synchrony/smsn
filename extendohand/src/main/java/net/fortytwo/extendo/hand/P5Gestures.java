package net.fortytwo.extendo.hand;

import com.illposed.osc.OSCListener;
import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPortIn;
import net.fortytwo.flow.NullSink;
import net.fortytwo.flow.Sink;
import net.fortytwo.myotherbrain.speech.Speaker;
import net.fortytwo.ripple.RippleException;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class P5Gestures {
    private int PORT = 1331;

    private Map<String, Command> commands = new HashMap<String, Command>();
    private Map<String, Command> abbreviations = new HashMap<String, Command>();

    private Sink<OSCMessage> nullSink = new NullSink<OSCMessage>();
    private Sink<OSCMessage> debugSink = new DebugSink();
    private RedirectableSink<OSCMessage> messageSink = new RedirectableSink<OSCMessage>(nullSink);

    private P5Vector lastSample;
    private Map<String, P5Vector> namedSamples = new HashMap<String, P5Vector>();

    private final Speaker speaker = new Speaker();

    public static void main(final String[] args) throws Exception {
        P5Gestures g = new P5Gestures();

        g.startListener();
        g.startIOLoop();
    }

    private P5Gestures() {
        messageSink.setDownstreamSink(nullSink);

        registerCommand(new ClassifyCommand());
        registerCommand(new LogCommand());
        registerCommand(new DefineCommand());
        registerCommand(new ExportCommand());
        registerCommand(new ImportCommand());
        registerCommand(new PrintCommand());
        registerCommand(new RealtimeCommand());
        registerCommand(new SampleCommand());
        registerCommand(new UndefineCommand());
    }

    private void registerCommand(final Command command) {
        commands.put(command.getName(), command);
        abbreviations.put(command.getName().substring(0, 1), command);
    }

    private void startListener() {
        Thread t = new Thread() {
            public void run() {
                try {
                    System.out.println("listening on port " + PORT);
                    OSCPortIn receiver = new OSCPortIn(PORT);
                    OSCListener listener = new OSCListener() {
                        public void acceptMessage(java.util.Date time, OSCMessage message) {
                            try {
                                messageSink.put(message);
                            } catch (Throwable e) {
                                System.err.println("error in OSC message handler");
                                e.printStackTrace(System.err);
                                System.exit(1);
                            }
                        }
                    };
                    receiver.addListener("/p5-out", listener);
                    receiver.startListening();
                } catch (Throwable t) {
                    System.err.println("error in OSC listener thread");
                    t.printStackTrace(System.err);
                    System.exit(1);
                }
            }
        };

        t.start();
    }

    private void startIOLoop() throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String line;

        List<String> commandNames = new LinkedList<String>();
        commandNames.addAll(commands.keySet());
        commandNames.add("quit");
        Collections.sort(commandNames);
        StringBuilder sb = new StringBuilder("commands: ");
        boolean first = true;
        for (String name : commandNames) {
            if (first) {
                first = false;
            } else {
                sb.append(", ");
            }

            sb.append(name);
        }

        System.out.println(sb);
        System.out.print("input> ");
        while (null != (line = br.readLine())) {
            line = line.replaceAll("\\s+", " ").trim();
            if (0 == line.length()) {
                continue;
            }

            String[] tokens = line.split(" ");

            String name = tokens[0];

            if (name.equals("quit")) {
                System.out.println("exiting");
                break;
            }

            Command c = commands.get(name);

            if (null == c) {
                c = abbreviations.get(name);
            }

            if (null == c) {
                System.out.println("unknown command: " + name);
            } else {
                try {
                    c.execute(tokens, br);
                } catch (CommandException e) {
                    System.out.println("error: " + e.getMessage());
                }
            }

            System.out.print("input> ");
        }
    }

    private static class CommandException extends Exception {
        public CommandException(final String message) {
            super(message);
        }
    }

    private abstract class Command {
        private String[] args;
        private BufferedReader reader;

        protected abstract void execute() throws CommandException;

        protected abstract int arity();

        public abstract String getName();

        public void execute(final String[] args,
                            final BufferedReader reader) throws CommandException {
            if (args.length < arity() + 1) {
                throw new CommandException("missing arguments");
            } else if (args.length > arity() + 1) {
                throw new CommandException("too many arguments");
            }

            this.args = args;
            this.reader = reader;
            execute();
        }

        protected String getArgument(final int index) throws CommandException {
            return args[index + 1];
        }

        protected void waitForEnter() {
            try {
                reader.readLine();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    }


    private class SampleCommand extends Command {
        protected void execute() throws CommandException {
            SampleSink s = new SampleSink();
            messageSink.setDownstreamSink(s);

            System.out.println("Sampling... press ENTER to stop and compute average");
            waitForEnter();

            P5Vector avg = s.getAverage();

            messageSink.setDownstreamSink(nullSink);

            System.out.println("\tdone");

            lastSample = avg;
        }

        protected int arity() {
            return 0;
        }

        public String getName() {
            return "sample";
        }
    }

    private class LogCommand extends Command {
        protected void execute() throws CommandException {
            messageSink.setDownstreamSink(debugSink);

            System.out.println("Logging OSC messages... press ENTER to stop");
            waitForEnter();

            messageSink.setDownstreamSink(nullSink);

            System.out.println("\tdone");
        }

        protected int arity() {
            return 0;
        }

        public String getName() {
            return "log";
        }
    }

    private class DefineCommand extends Command {
        protected void execute() throws CommandException {
            if (null == lastSample) {
                throw new CommandException("you have not yet taken a sample");
            } else {
                String name = getArgument(0);
                namedSamples.put(name, lastSample);
                System.out.println("defined '" + name + "' as last sample");
            }
        }

        protected int arity() {
            return 1;
        }

        public String getName() {
            return "define";
        }
    }

    private class PrintCommand extends Command {
        protected void execute() throws CommandException {
            String name = getArgument(0);

            P5Vector v = namedSamples.get(name);

            if (null == v) {
                throw new CommandException("there is no sample named '" + name + "'");
            } else {
                System.out.println("sample '" + name + "': " + v);
            }
        }

        protected int arity() {
            return 1;
        }

        public String getName() {
            return "print";
        }
    }

    private class RealtimeCommand extends Command {

        protected void execute() throws CommandException {
            messageSink.setDownstreamSink(new RealtimeSink());

            System.out.println("Sampling and displaying best matches in real time... press ENTER to stop");
            waitForEnter();

            messageSink.setDownstreamSink(nullSink);
            System.out.println("\tdone");
        }

        protected int arity() {
            return 0;
        }

        public String getName() {
            return "realtime";
        }
    }

    private class ExportCommand extends Command {
        protected void execute() throws CommandException {
            String fileName = getArgument(0);

            List<String> names = new LinkedList<String>();
            names.addAll(namedSamples.keySet());
            Collections.sort(names);

            StringBuilder sb = new StringBuilder();
            for (String name : names) {
                sb.append(name);
                P5Vector v = namedSamples.get(name);
                for (int i = 0; i < v.components.length; i++) {
                    sb.append("\t").append(v.components[i]);
                }
                sb.append("\n");
            }

            try {
                OutputStream out = new FileOutputStream(new File(fileName));
                try {
                    out.write(sb.toString().getBytes());
                } finally {
                    out.close();
                }
            } catch (IOException e) {
                throw new CommandException("file I/O error: " + e.getMessage());
            }

            System.out.println("exported " + namedSamples.size() + " symbols to file " + fileName);
        }

        protected int arity() {
            return 1;
        }

        public String getName() {
            return "export";
        }
    }

    private class ImportCommand extends Command {
        protected void execute() throws CommandException {
            int count = 0;

            String fileName = getArgument(0);

            try {
                InputStream in = new FileInputStream(new File(fileName));
                BufferedReader r = new BufferedReader(new InputStreamReader(in));

                String line;
                while (null != (line = r.readLine())) {
                    line = line.trim();

                    if (0 == line.length()) {
                        continue;
                    }

                    count++;
                    String[] parts = line.trim().split(" ");
                    if (parts.length < 2) {
                        throw new CommandException("empty on line: " + line);
                    }

                    String name = parts[0];
                    //System.out.println("defining " + name);
                    double[] c = new double[parts.length - 1];
                    for (int i = 1; i < parts.length; i++) {
                        try {
                            c[i - 1] = Double.valueOf(parts[i]);
                        } catch (NumberFormatException e) {
                            throw new CommandException("invalid number on line: " + line);
                        }
                    }

                    P5Vector v = new P5Vector(c);
                    namedSamples.put(name, v);
                }
            } catch (IOException e) {
                throw new CommandException("file I/O error: " + e.getMessage());
            }

            System.out.println("imported " + count + " symbols from file " + fileName);
        }

        protected int arity() {
            return 1;
        }

        public String getName() {
            return "import";
        }
    }

    private class UndefineCommand extends Command {
        protected void execute() throws CommandException {
            String name = getArgument(0);

            namedSamples.put(name, null);
            System.out.println("undefined '" + name + "'");
        }

        protected int arity() {
            return 1;
        }

        public String getName() {
            return "undefine";
        }
    }

    private String classify(final P5Vector vector) throws CommandException {
        if (0 == namedSamples.size()) {
            throw new CommandException("no vectors have been defined yet");
        }

        String best = null;
        double min = -1;

        for (Map.Entry<String, P5Vector> e : namedSamples.entrySet()) {
            double dist = vector.distanceFrom(e.getValue());

            if (min < 0 || dist < min) {
                best = e.getKey();
                min = dist;
            }
        }

        return best;
    }

    private class ClassifyCommand extends Command {
        protected void execute() throws CommandException {
            if (null == lastSample) {
                throw new CommandException("you have not yet taken a sample");
            } else {
                String best = classify(lastSample);

                System.out.println("closest match is '" + best + "'");
            }
        }

        protected int arity() {
            return 0;
        }

        public String getName() {
            return "classify";
        }
    }

    private static class P5Vector implements Cloneable {
        public final double[] components;

        public P5Vector(final int dimensions) {
            components = new double[dimensions];
            for (int i = 0; i < components.length; i++) {
                components[i] = 0;
            }
        }

        public P5Vector(double... components) {
            this.components = components;
        }

        public P5Vector(final P5Vector other) {
            this(other.components.clone());
        }

        /*public void add(final P5Vector other) {
            for (int i = 0; i < components.length; i++) {
                components[i] += other.components[i];
            }
        }*/

        public void multiplyBy(final double scalar) {
            for (int i = 0; i < components.length; i++) {
                components[i] *= scalar;
            }
        }

        public double distanceFrom(final P5Vector other) {
            double sum = 0;
            for (int i = 0; i < components.length; i++) {
                double diff = components[i] - other.components[i];
                sum += diff * diff;
            }

            return Math.sqrt(sum);
        }

        public String toString() {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < components.length; i++) {
                if (0 != i) {
                    sb.append(" ");
                }

                sb.append(components[i]);
            }

            return sb.toString();
        }
    }

    private class DebugSink implements Sink<OSCMessage> {
        public void put(OSCMessage message) throws RippleException {
            StringBuilder sb = new StringBuilder();
            for (Object a : message.getArguments()) {
                sb.append("\t").append(a);
            }
            System.out.println(sb);
        }
    }

    private class RealtimeSink implements Sink<OSCMessage> {
        private P5Vector vector = new P5Vector(5);
        private String lastMatch = "";
        private String lastSpoken = "";
        private long lastChange = System.currentTimeMillis();

        public void put(final OSCMessage message) throws RippleException {
            int i = 0;
            for (Object a : message.getArguments()) {
                vector.components[i] = new Double(a.toString());
                i++;
            }

            try {
                String current = classify(vector);
                System.out.println("current best match: " + current);

                long now = System.currentTimeMillis();

                if (!current.equals(lastMatch)) {
                    lastMatch = current;
                    lastChange = now;
                }

                if (!current.equals(lastSpoken) && now > 1000 + lastChange) {
                    speaker.speak(current);
                    lastSpoken = current;
                }
            } catch (CommandException e) {
                System.out.println("error: " + e.getMessage());
            }
        }
    }

    private class SampleSink implements Sink<OSCMessage> {
        private final P5Vector sum = new P5Vector(5);
        private int count = 0;

        public void put(OSCMessage message) throws RippleException {
            int i = 0;
            for (Object a : message.getArguments()) {
                sum.components[i] += new Double(a.toString());
                i++;
            }
            count++;
        }

        public P5Vector getAverage() {
            P5Vector avg = new P5Vector(sum);
            if (count > 0) {
                avg.multiplyBy(1f / count);
            }
            return avg;
        }
    }

    private class RedirectableSink<T> implements Sink<T> {
        private Sink<T> downstreamSink;

        public RedirectableSink(final Sink<T> downstreamSink) {
            this.downstreamSink = downstreamSink;
        }

        public void setDownstreamSink(final Sink<T> downstreamSink) {
            this.downstreamSink = downstreamSink;
        }

        public void put(T t) throws RippleException {
            downstreamSink.put(t);
        }
    }
}
