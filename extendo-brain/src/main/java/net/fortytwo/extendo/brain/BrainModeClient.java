package net.fortytwo.extendo.brain;

import info.aduna.io.IOUtil;
import org.apache.commons.lang.StringEscapeUtils;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BrainModeClient {

    private final InputStream inputstream;

    private enum State {TEXT, EVENT, COMMAND, ARGUMENT, ESCAPE}

    private final Map<String, EmacsFunction> functions;
    // a sorted array of keyboard shortcuts
    private final String[] shortcuts;

    private int shortcutIndex;

    private final StringBuilder textBuffer;
    private final StringBuilder commandBuffer;

    private State state;
    private State lastState;
    private EmacsFunction currentFunction;

    private final EmacsFunction insertFunction = new EmacsFunction("insert", true);

    private String executable = "emacsclient";

    private EmacsFunctionExecutor functionExecutor = new EmacsFunctionExecutor() {
        public Process execute(EmacsFunction function, String argument) throws InterruptedException, IOException {
            String expr = function.getRequiresArgument()
                    ? "(" + function.getName() + " \"" + StringEscapeUtils.escapeJava(argument) + "\")"
                    : "(" + function.getName() + ")";
            expr = "(exo-emacsclient-eval (lambda () " + expr + "))";

            Process p = Runtime.getRuntime().exec(new String[]{executable, "-e", expr});
            p.waitFor();

            return p;
        }
    };

    public BrainModeClient(InputStream inputstream) {
        this.inputstream = inputstream;

        textBuffer = new StringBuilder();
        commandBuffer = new StringBuilder();

        functions = new HashMap<String, EmacsFunction>();

        // general Emacs functions
        functions.put("C-a", new EmacsFunction("move-beginning-of-line", false));
        functions.put("C-e", new EmacsFunction("move-end-of-line", false));
        functions.put("C-g C-g C-g", new EmacsFunction("keyboard-escape-quit", false));
        functions.put("C-k", new EmacsFunction("kill-line", false));
        functions.put("C-x k", new EmacsFunction("kill-buffer", false));

        // shortcuts based on Mac command key as "super" key, e.g. using (setq mac-command-modifier 'super)
        functions.put("s-c", new EmacsFunction("clipboard-kill-ring-save", false));
        functions.put("s-v", new EmacsFunction("clipboard-yank", false));
        functions.put("s-z", new EmacsFunction("undo", false));
        functions.put("s-Z", new EmacsFunction("redo", false));

        functions.put("DEL", new EmacsFunction("delete-backward-char 1", false));
        // TODO: ESC is currently a no-op...
        functions.put("ESC", new EmacsFunction("+40 2", false));
        //functions.put("RET", ...
        //functions.put("SPACE", ...
        functions.put("up", new EmacsFunction("previous-line", false));
        functions.put("down", new EmacsFunction("next-line", false));
        functions.put("left", new EmacsFunction("backward-char", false));
        functions.put("right", new EmacsFunction("forward-char", false));

        // Brain-mode specific functions
        functions.put("C-c C-a C-p", new EmacsFunction("exo-insert-attr-priority", true));
        functions.put("C-c C-a C-s", new EmacsFunction("exo-insert-attr-sharability", true));
        functions.put("C-c C-a C-w", new EmacsFunction("exo-insert-attr-weight", true));
        functions.put("C-c C-a d", new EmacsFunction("exo-insert-current-date", false));
        functions.put("C-c C-a s", new EmacsFunction("exo-insert-current-time-with-seconds", false));
        functions.put("C-c C-a t", new EmacsFunction("exo-insert-current-time", false));
        functions.put("C-c C-d", new EmacsFunction("exo-choose-depth", true));
        functions.put("C-c C-l", new EmacsFunction("exo-goto-line", true));
        functions.put("C-c C-r C-b a", new EmacsFunction("exo-visit-in-amazon 'current-root-value", false));
        functions.put("C-c C-r C-b e", new EmacsFunction("exo-visit-in-ebay 'current-root-value", false));
        functions.put("C-c C-r C-b d", new EmacsFunction("exo-visit-in-delicious 'current-root-value", false));
        functions.put("C-c C-r C-b g", new EmacsFunction("exo-visit-in-google 'current-root-value", false));
        functions.put("C-c C-r C-b m", new EmacsFunction("exo-visit-in-google-maps 'current-root-value", false));
        functions.put("C-c C-r C-b s", new EmacsFunction("exo-visit-in-scholar 'current-root-value", false));
        functions.put("C-c C-r C-b t", new EmacsFunction("exo-visit-in-twitter 'current-root-value", false));
        functions.put("C-c C-r C-b w", new EmacsFunction("exo-visit-in-wikipedia 'current-root-value", false));
        functions.put("C-c C-r C-b y", new EmacsFunction("exo-visit-in-youtube 'current-root-value", false));
        functions.put("C-c C-s C-d", new EmacsFunction("exo-set-default-sharability", true));
        functions.put("C-c C-s C-m", new EmacsFunction("exo-set-min-sharability", true));
        functions.put("C-c C-t C-a b", new EmacsFunction("exo-visit-target-alias", false));
        functions.put("C-c C-t C-b a", new EmacsFunction("exo-visit-in-amazon 'current-target-value", false));
        functions.put("C-c C-t C-b e", new EmacsFunction("exo-visit-in-ebay 'current-target-value", false));
        functions.put("C-c C-t C-b d", new EmacsFunction("exo-visit-in-delicious 'current-target-value", false));
        functions.put("C-c C-t C-b g", new EmacsFunction("exo-visit-in-google 'current-target-value", false));
        functions.put("C-c C-t C-b m", new EmacsFunction("exo-visit-in-google-maps 'current-target-value", false));
        functions.put("C-c C-t C-b s", new EmacsFunction("exo-visit-in-scholar 'current-target-value", false));
        functions.put("C-c C-t C-b t", new EmacsFunction("exo-visit-in-twitter 'current-target-value", false));
        functions.put("C-c C-t C-b w", new EmacsFunction("exo-visit-in-wikipedia 'current-target-value", false));
        functions.put("C-c C-t C-b y", new EmacsFunction("exo-visit-in-youtube 'current-target-value", false));
        functions.put("C-c C-t C-p", new EmacsFunction("exo-set-target-priority", true));
        functions.put("C-c C-t C-s", new EmacsFunction("exo-set-target-sharability", true));
        functions.put("C-c C-t C-w", new EmacsFunction("exo-set-target-weight", true));
        functions.put("C-c C-t a", new EmacsFunction("exo-visit-as-url 'current-target-value", false));
        functions.put("C-c C-t c", new EmacsFunction("exo-copy-target-value-to-clipboard", false));
        functions.put("C-c C-t i", new EmacsFunction("exo-atom-info 'current-target", false));
        functions.put("C-c C-t l", new EmacsFunction("exo-preview-target-latex-math", false));
        functions.put("C-c C-t r", new EmacsFunction("exo-copy-target-reference-to-clipboard", false));
        functions.put("C-c C-v ;", new EmacsFunction("exo-toggle-truncate-lines", false));
        functions.put("C-c C-v b", new EmacsFunction("exo-update-to-backward-view", false));
        functions.put("C-c C-v e", new EmacsFunction("exo-enter-edit-view", false));
        functions.put("C-c C-v f", new EmacsFunction("exo-update-to-forward-view", false));
        functions.put("C-c C-v i", new EmacsFunction("exo-toggle-inference-viewstyle", false));
        functions.put("C-c C-v r", new EmacsFunction("exo-enter-readonly-view", false));
        functions.put("C-c C-v s", new EmacsFunction("exo-toggle-emacspeak", false));
        functions.put("C-c C-v t", new EmacsFunction("exo-set-value-truncation-length", true));
        functions.put("C-c C-w C-d", new EmacsFunction("exo-set-default-weight", true));
        functions.put("C-c C-w C-m", new EmacsFunction("exo-set-min-weight", true));
        functions.put("C-c a", new EmacsFunction("exo-visit-url-at-point", false));
        functions.put("C-c d", new EmacsFunction("exo-duplicates", false));
        functions.put("C-c e", new EmacsFunction("exo-export", false));
        functions.put("C-c f", new EmacsFunction("exo-find-roots", false));
        functions.put("C-c h", new EmacsFunction("exo-history", false));
        functions.put("C-c i", new EmacsFunction("exo-infer-types", false));
        functions.put("C-c P", new EmacsFunction("exo-priorities", false));
        functions.put("C-c p", new EmacsFunction("exo-push-view", false));
        functions.put("C-c r", new EmacsFunction("exo-ripple-query", true));
        functions.put("C-c s", new EmacsFunction("exo-search", true));
        functions.put("C-c t", new EmacsFunction("exo-visit-target", false));
        functions.put("C-c u", new EmacsFunction("exo-update-view", false));
        functions.put("C-c v", new EmacsFunction("exo-events", false));

        shortcuts = new String[functions.size()];
        int i = 0;
        for (String s : functions.keySet()) {
            shortcuts[i++] = s;
        }
        Arrays.sort(shortcuts);
    }

    public void setExecutable(final String executable) {
        this.executable = executable;
    }

    public void setFunctionExecutor(final EmacsFunctionExecutor executor) {
        this.functionExecutor = executor;
    }

    public void run() throws IOException, InterruptedException, ExecutionException {
        // note: this call to reset() makes it unnecessary to reset() before throwing parse errors
        reset();

        int c;
        while (-1 != (c = inputstream.read())) {
            switch (state) {
                case TEXT:
                    if ('<' == c) {
                        finishText();
                        state = State.EVENT;
                    } else if ('>' == c) {
                        throw new IOException("mismatched '>'");
                    } else if ('\\' == c) {
                        lastState = state;
                        state = State.ESCAPE;
                    } else if ('\n' == c) {
                        // send text every time there is a newline, even if no other events have occurred
                        textBuffer.append('\n');
                        finishText();
                    } else {
                        textBuffer.append((char) c);
                    }
                    break;
                case EVENT:
                    if ('>' == c) {
                        state = matchCommand();
                    } else if ('\\' == c) {
                        throw new IOException("escape character '\\' in event <...>");
                    } else if ('<' == c) {
                        throw new IOException("mismatched '<'");
                    } else if ('\n' == c) {
                        throw new IOException("newline in event <...>");
                    } else {
                        textBuffer.append((char) c);
                    }
                    break;
                case COMMAND:
                    if ('<' == c) {
                        state = State.EVENT;
                    } else if ('>' == c) {
                        throw new IOException("mismatched '>'");
                    } else if ('\\' == c) {
                        lastState = state;
                        state = State.ESCAPE;
                    } else if ('\n' == c) {
                        throw new IOException("no such command: " + commandBuffer.toString());
                    } else {
                        textBuffer.append((char) c);
                        state = matchCommand();
                    }
                    break;
                case ARGUMENT:
                    if ('<' == c) {
                        // simply abandon the command in progress; this is simpler than Emacs behavior,
                        // but allows one to escape from a partially-completed command with C-g C-g C-g, for example
                        reset();
                        state = State.EVENT;
                    } else if ('>' == c) {
                        throw new IOException("'>' found outside of command");
                    } else if ('\\' == c) {
                        lastState = state;
                        state = State.ESCAPE;
                    } else if ('\n' == c) { // newline signals the end of an interactive argument
                        execute(currentFunction, textBuffer.toString());
                        reset();
                    } else {
                        textBuffer.append((char) c);
                    }
                    break;
                case ESCAPE:
                    state = lastState;
                    textBuffer.append((char) c);
                    break;
            }
        }
    }

    private void reset() {
        textBuffer.setLength(0);
        commandBuffer.setLength(0);
        shortcutIndex = 0;
        state = State.TEXT;

        // TODO: not necessary; just for looks
        currentFunction = null;
        lastState = null;
    }

    private void finishText() throws IOException, InterruptedException, ExecutionException {
        if (textBuffer.length() > 0) {
            String text = textBuffer.toString();
            textBuffer.setLength(0);

            writeTextBuffer(text);
        }
    }

    private State matchCommand() throws IOException, InterruptedException, ExecutionException {
        if (textBuffer.length() > 0) {
            String event = textBuffer.toString();
            textBuffer.setLength(0);

            if (commandBuffer.length() > 0) {
                commandBuffer.append(" ");
            }

            commandBuffer.append(event);

            String command = commandBuffer.toString();
            //System.err.println("matching command " + command + " from " + shortcuts[shortcutIndex]);
            // exit when a partial or complete match is found or the command is found not to match
            // TODO: use binary search for efficiency
            while (true) {
                String cur = shortcuts[shortcutIndex];
                int cmp = command.compareTo(cur);
                //System.err.println("\t" + cmp + " for " + cur);
                if (0 == cmp) {
                    //System.err.println("\t" + cur + " is a match");
                    EmacsFunction f = functions.get(command);
                    if (null == f) {
                        throw new IllegalStateException();
                    }
                    //System.err.println("\trequires argument: " + f.requiresArgument);

                    if (f.getRequiresArgument()) {
                        textBuffer.setLength(0);
                        currentFunction = f;

                        return State.ARGUMENT;
                    } else {
                        execute(f, null);
                        reset();
                        return State.TEXT;
                    }
                } else if (cmp < 0) {
                    if (cur.startsWith(command)) {
                        return State.COMMAND;
                    } else {
                        throw new IOException("unknown command: " + command);
                    }
                } else {
                    shortcutIndex++;
                    if (shortcuts.length == shortcutIndex) {
                        throw new IOException("unknown command: " + command);
                    }
                }
            }
        } else {
            throw new IOException("empty event: <>");
        }
    }

    private void writeTextBuffer(final String text) throws IOException, InterruptedException, ExecutionException {
        execute(insertFunction, text);
    }

    private void execute(final EmacsFunction f, final String text) throws IOException, InterruptedException, ExecutionException {
        Process p = functionExecutor.execute(f, text);

        if (null != p && 0 != p.exitValue()) {
            StringBuilder sb = new StringBuilder();
            sb.append("failed to execute emacs function ")
            .append(f.getName()).append(f.requiresArgument ? " with argument " + text : "")
            .append(" (exit code = ").append(p.exitValue()).append(").");

            byte[] in = IOUtil.readBytes(p.getInputStream());
            if (in.length > 0) {
                sb.append("\nInput: ");
                sb.append(new String(in));
            }

            byte[] out = IOUtil.readBytes(p.getErrorStream());
            if (out.length > 0) {
                sb.append("\nOutput: ");
                sb.append(new String(out));
            }

            throw new ExecutionException(sb.toString());
        }
    }

    public class EmacsFunction {
        private final String name;
        private final boolean requiresArgument;

        private EmacsFunction(final String name,
                              final boolean requiresArgument) {
            this.name = name;
            this.requiresArgument = requiresArgument;
        }

        public String getName() {
            return name;
        }

        public boolean getRequiresArgument() {
            return requiresArgument;
        }
    }

    public interface EmacsFunctionExecutor {
        Process execute(EmacsFunction function,
                        String argument) throws InterruptedException, IOException;
    }

    public class ExecutionException extends Exception {
        public ExecutionException(final String message) {
            super(message);
        }
    }
}
