package net.fortytwo.extendo.brain.wiki;

import net.fortytwo.extendo.brain.ExtendoBrain;
//import org.antlr.v4.runtime.CharStream;
//import org.antlr.v4.runtime.CommonTokenStream;
import org.junit.Test;

public class ExtendoBrainGrammarTest {
    @Test
    public void testNothing() throws Exception {
        //String []files = new String[]{"wiki-example-1.txt", "wiki-example-2.txt", "wiki-example-3.txt"};
        String []files = new String[]{"test.txt"};
        for (String f : files) {
            /* DO NOT REMOVE.  Uncomment when testing the "documentation" grammar.
            System.out.println("testing " + f); System.out.flush();
            CharStream cStream = new org.antlr.v4.runtime.ANTLRInputStream(ExtendoBrain.class.getResourceAsStream(f));
            ExtendoBrainLexer lexer = new ExtendoBrainLexer(cStream);
            ExtendoBrainParser parser = new ExtendoBrainParser(new CommonTokenStream(lexer));

            ExtendoBrainParser.DocumentContext context = parser.document();
            */
        }
    }
}
