package net.fortytwo.smsn.brain;

import info.aduna.io.IOUtil;
import org.apache.commons.lang.StringEscapeUtils;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * A lightweight, Java-based client for Brain-mode, which is in turn the Emacs Lisp client of
 * the Extend-o-Brain personal knowledge base
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BrainModeClient {

    private final Logger logger = Logger.getLogger(BrainModeClient.class.getName());

    private final InputStream inputstream;

    private enum State {CHAR, ENTITY, COMMAND, ARGUMENT, ESCAPE}

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

    private final ResultHandler resultHandler;

    private EmacsFunctionExecutor functionExecutor = (function, argument) -> {
        String expr = function.getRequiresArgument()
                ? "(" + function.getName() + " \"" + StringEscapeUtils.escapeJava(argument) + "\")"
                : "(" + function.getName() + ")";
        expr = "(smsn-emacsclient-eval (lambda () " + expr + "))";

        Process p = Runtime.getRuntime().exec(new String[]{executable, "-e", expr});
        p.waitFor();

        return p;
    };

    public BrainModeClient(final InputStream inputstream,
                           final ResultHandler resultHandler) {
        this.inputstream = inputstream;
        this.resultHandler = resultHandler;

        textBuffer = new StringBuilder();
        commandBuffer = new StringBuilder();

        functions = new HashMap<>();

        // general Emacs functions
        functions.put("C-a", new EmacsFunction("move-beginning-of-line 1", false));
        functions.put("C-e", new EmacsFunction("move-end-of-line 1", false));
        functions.put("C-g C-g C-g", new EmacsFunction("keyboard-escape-quit", false));
        functions.put("C-k", new EmacsFunction("kill-line", false));
        //functions.put("C-x C-e", new EmacsFunction("eval-last-sexp", false));
        functions.put("C-x k", new EmacsFunction("kill-buffer", false));

        // shortcuts based on Mac command key as "super" key, e.g. using (setq mac-command-modifier 'super)
        functions.put("s-c", new EmacsFunction("clipboard-kill-ring-save", false));
        functions.put("s-v", new EmacsFunction("clipboard-yank", false));
        functions.put("s-z", new EmacsFunction("undo", false));
        functions.put("s-Z", new EmacsFunction("redo", false));

        functions.put("DEL", new EmacsFunction("delete-backward-char 1", false));
        // TODO: ESC is currently a no-op...
        functions.put("ESC", new EmacsFunction("+40 2", false));
        /*
        functions.put("up", new EmacsFunction("previous-line", false));
        functions.put("down", new EmacsFunction("next-line", false));
        functions.put("left", new EmacsFunction("backward-char", false));
        functions.put("right", new EmacsFunction("forward-char", false));
        */
        functions.put("up", new EmacsFunction("smsn-previous-line", false));
        functions.put("down", new EmacsFunction("smsn-next-line", false));
        functions.put("left", new EmacsFunction("smsn-backward-char", false));
        functions.put("right", new EmacsFunction("smsn-forward-char", false));

        // Brain-mode specific functions
        functions.put("C-c C-I f", new EmacsFunction("smsn-find-isolated-atoms", false));
        functions.put("C-c C-I r", new EmacsFunction("smsn-remove-isolated-atoms", false));
        functions.put("C-c C-a C-p", new EmacsFunction("smsn-insert-attr-priority", true));
        functions.put("C-c C-a C-s", new EmacsFunction("smsn-insert-attr-sharability", true));
        functions.put("C-c C-a C-w", new EmacsFunction("smsn-insert-attr-weight", true));
        functions.put("C-c C-a d", new EmacsFunction("smsn-insert-current-date", false));
        functions.put("C-c C-a s", new EmacsFunction("smsn-insert-current-time-with-seconds", false));
        functions.put("C-c C-a t", new EmacsFunction("smsn-insert-current-time", false));
        functions.put("C-c C-d", new EmacsFunction("smsn-set-view-height", true));
        // TODO: export and import functions
        functions.put("C-c C-l", new EmacsFunction("smsn-goto-line", true));
        functions.put("C-c C-r C-b a", new EmacsFunction("smsn-visit-in-amazon 'current-root-value", false));
        functions.put("C-c C-r C-b e", new EmacsFunction("smsn-visit-in-ebay 'current-root-value", false));
        functions.put("C-c C-r C-b d", new EmacsFunction("smsn-visit-in-delicious 'current-root-value", false));
        functions.put("C-c C-r C-b g", new EmacsFunction("smsn-visit-in-google 'current-root-value", false));
        functions.put("C-c C-r C-b m", new EmacsFunction("smsn-visit-in-google-maps 'current-root-value", false));
        functions.put("C-c C-r C-b s", new EmacsFunction("smsn-visit-in-scholar 'current-root-value", false));
        functions.put("C-c C-r C-b t", new EmacsFunction("smsn-visit-in-twitter 'current-root-value", false));
        functions.put("C-c C-r C-b w", new EmacsFunction("smsn-visit-in-wikipedia 'current-root-value", false));
        functions.put("C-c C-r C-b y", new EmacsFunction("smsn-visit-in-youtube 'current-root-value", false));
        functions.put("C-c C-s C-d", new EmacsFunction("smsn-set-default-sharability", true));
        functions.put("C-c C-s C-m", new EmacsFunction("smsn-set-min-sharability", true));
        functions.put("C-c C-t C-a b", new EmacsFunction("smsn-visit-target-alias", false));
        functions.put("C-c C-t C-b a", new EmacsFunction("smsn-visit-in-amazon 'current-target-value", false));
        functions.put("C-c C-t C-b e", new EmacsFunction("smsn-visit-in-ebay 'current-target-value", false));
        functions.put("C-c C-t C-b d", new EmacsFunction("smsn-visit-in-delicious 'current-target-value", false));
        functions.put("C-c C-t C-b g", new EmacsFunction("smsn-visit-in-google 'current-target-value", false));
        functions.put("C-c C-t C-b m", new EmacsFunction("smsn-visit-in-google-maps 'current-target-value", false));
        functions.put("C-c C-t C-b s", new EmacsFunction("smsn-visit-in-scholar 'current-target-value", false));
        functions.put("C-c C-t C-b t", new EmacsFunction("smsn-visit-in-twitter 'current-target-value", false));
        functions.put("C-c C-t C-b w", new EmacsFunction("smsn-visit-in-wikipedia 'current-target-value", false));
        functions.put("C-c C-t C-b y", new EmacsFunction("smsn-visit-in-youtube 'current-target-value", false));
        functions.put("C-c C-t C-p", new EmacsFunction("smsn-set-target-priority", true));
        functions.put("C-c C-t C-s", new EmacsFunction("smsn-set-target-sharability", true));
        functions.put("C-c C-t C-w", new EmacsFunction("smsn-set-target-weight", true));
        functions.put("C-c C-t a", new EmacsFunction("smsn-visit-as-url 'current-target-value", false));
        functions.put("C-c C-t c", new EmacsFunction("smsn-copy-target-value-to-clipboard", false));
        functions.put("C-c C-t i", new EmacsFunction("smsn-atom-info 'current-target", false));
        functions.put("C-c C-t l", new EmacsFunction("smsn-preview-target-latex-math", false));
        functions.put("C-c C-t r", new EmacsFunction("smsn-copy-target-reference-to-clipboard", false));
        functions.put("C-c C-v ;", new EmacsFunction("smsn-toggle-truncate-lines", false));
        functions.put("C-c C-v b", new EmacsFunction("smsn-update-to-backward-view", false));
        functions.put("C-c C-v e", new EmacsFunction("smsn-enter-edit-view", false));
        functions.put("C-c C-v f", new EmacsFunction("smsn-update-to-forward-view", false));
        functions.put("C-c C-v i", new EmacsFunction("smsn-toggle-inference-viewstyle", false));
        functions.put("C-c C-v p", new EmacsFunction("smsn-toggle-properties-view", false));
        functions.put("C-c C-v r", new EmacsFunction("smsn-enter-readonly-view", false));
        functions.put("C-c C-v s", new EmacsFunction("smsn-toggle-emacspeak", false));
        functions.put("C-c C-v t", new EmacsFunction("smsn-set-value-truncation-length", true));
        functions.put("C-c C-v v", new EmacsFunction("smsn-toggle-minimize-verbatim-blocks", false));
        functions.put("C-c C-w C-d", new EmacsFunction("smsn-set-default-weight", true));
        functions.put("C-c C-w C-m", new EmacsFunction("smsn-set-min-weight", true));
        functions.put("C-c a", new EmacsFunction("smsn-acronym-query", true));
        functions.put("C-c b", new EmacsFunction("smsn-visit-url-at-point", false));
        functions.put("C-c d", new EmacsFunction("smsn-duplicates", false));
        functions.put("C-c e", new EmacsFunction("smsn-export", false));
        functions.put("C-c f", new EmacsFunction("smsn-find-roots", false));
        functions.put("C-c h", new EmacsFunction("smsn-history", false));
        functions.put("C-c i", new EmacsFunction("smsn-infer-types", false));
        functions.put("C-c P", new EmacsFunction("smsn-priorities", false));
        functions.put("C-c p", new EmacsFunction("smsn-push-view", false));
        functions.put("C-c r", new EmacsFunction("smsn-ripple-query", true));
        functions.put("C-c s", new EmacsFunction("smsn-fulltext-query", true));
        functions.put("C-c t", new EmacsFunction("smsn-visit-target", false));
        functions.put("C-c u", new EmacsFunction("smsn-update-view", false));
        functions.put("C-c v", new EmacsFunction("smsn-events", false));

        // extended Brain-mode mappings, not available as Emacs key mappings
        functions.put("C-c c", new EmacsFunction("atom-id-at-point", false));

        // Emacspeak functions, not available in all environments
        // Note that the actual Emacspeak shortcuts begin with C-e
        // We prepend C-c to avoid conflict with move-end-of-line
        functions.put("C-c C-e c", new EmacsFunction("emacspeak-speak-char", false));
        functions.put("C-c C-e l", new EmacsFunction("emacspeak-speak-line", false));
        functions.put("C-c C-e w", new EmacsFunction("emacspeak-speak-word", false));

        shortcuts = new String[functions.size()];
        int i = 0;
        for (String s : functions.keySet()) {
            shortcuts[i++] = s;
        }
        Arrays.sort(shortcuts);

        reset();
    }

    public void setExecutable(final String executable) {
        this.executable = executable;
    }

    public void setFunctionExecutor(final EmacsFunctionExecutor executor) {
        this.functionExecutor = executor;
    }

    // note: the client maintains state even between (non-parse) errors,
    // allowing it to recover as soon as possible from the failed execution of a valid expression.
    // When a parse error is encountered, the state is reset to CHAR.
    public void run() throws IOException, InterruptedException, ExecutionException, UnknownCommandException, ParseError {
        int c;
        while (-1 != (c = inputstream.read())) {
            //System.out.println("read: " + (char) c + " in state " + state);
            switch (state) {
                case CHAR:
                    if ('<' == c) {
                        state = State.ENTITY;
                        finishText();
                    } else if ('>' == c) {
                        reset();
                        throw new ParseError("mismatched '>' in " + state);
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
                case ENTITY:
                    if ('>' == c) {
                        state = matchCommand();
                    } else if ('\\' == c) {
                        reset();
                        throw new ParseError("escape character '\\' in " + state);
                    } else if ('<' == c) {
                        reset();
                        throw new ParseError("mismatched '<' in " + state);
                    } else if ('\n' == c) {
                        reset();
                        throw new ParseError("newline in " + state);
                    } else {
                        textBuffer.append((char) c);
                    }
                    break;
                case COMMAND:
                    if ('<' == c) {
                        state = State.ENTITY;
                    } else if ('>' == c) {
                        reset();
                        throw new ParseError("mismatched '>' in " + state);
                    } else if ('\\' == c) {
                        lastState = state;
                        state = State.ESCAPE;
                    } else if ('\n' == c) {
                        reset();
                        throw new ParseError("no such command: " + commandBuffer.toString());
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
                        state = State.ENTITY;
                    } else if ('>' == c) {
                        reset();
                        throw new ParseError("'>' found outside of command in " + state);
                    } else if ('\\' == c) {
                        lastState = state;
                        state = State.ESCAPE;
                    } else if ('\n' == c) { // newline signals the end of an interactive argument
                        try {
                            execute(currentFunction, textBuffer.toString(), true);
                        } finally {
                            reset();
                        }
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
        state = State.CHAR;

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

    private State matchCommand() throws IOException, InterruptedException, ExecutionException, UnknownCommandException, ParseError {
        if (textBuffer.length() > 0) {
            String entity = textBuffer.toString();
            textBuffer.setLength(0);

            if (commandBuffer.length() > 0) {
                commandBuffer.append(" ");
            }

            commandBuffer.append(entity);

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
                        try {
                            execute(f, null, true);
                        } finally {
                            reset();
                        }
                        return State.CHAR;
                    }
                } else if (cmp < 0) {
                    if (cur.startsWith(command)) {
                        return State.COMMAND;
                    } else {
                        throw new UnknownCommandException(command);
                    }
                } else {
                    shortcutIndex++;
                    if (shortcuts.length == shortcutIndex) {
                        throw new IOException("unknown command: " + command);
                    }
                }
            }
        } else {
            throw new ParseError("empty entity: <>");
        }
    }

    private void writeTextBuffer(final String text) throws IOException, InterruptedException, ExecutionException {
        execute(insertFunction, text, false);
    }

    private void execute(final EmacsFunction f, final String text, final boolean doHandle)
            throws IOException, InterruptedException, ExecutionException {

        Process p = functionExecutor.execute(f, text);
        if (null == p) {
            return;
        }

        if (0 != p.exitValue()) {
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
        } else if (doHandle) {
            try {
                resultHandler.handle(p.getInputStream());
            } catch (Throwable t) {
                logger.log(Level.SEVERE, "failed to handle Brain-mode client result", t);
            }
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
                        String argument) throws IOException, InterruptedException;
    }

    public interface ResultHandler {
        void handle(InputStream result);
    }

    public class ParseError extends Exception {
        public ParseError(final String message) {
            super(message);
        }
    }

    /**
     * A failure to execute an Emacs command, with reasons ranging in severity from a missing server to
     * a read-only buffer.
     */
    public class ExecutionException extends Exception {
        public ExecutionException(final String message) {
            super(message);
        }
    }

    /**
     * A failure due to a missing command.  If not arising from a simple typo, it may have to do with the limited
     * set of commands supported by the client vis-a-vis the unlimited set of Emacs key bindings.
     */
    public class UnknownCommandException extends Exception {
        public UnknownCommandException(final String command) {
            super(command);
        }
    }
}
