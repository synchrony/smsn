package net.fortytwo.extendo.util;

import java.math.BigDecimal;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Design {
    public static void main(final String[] args) throws Exception {
        int base = 64;
        int length = 7;

        BigDecimal poss = new BigDecimal(base).pow(length);
        BigDecimal trials = new BigDecimal(Math.sqrt((double) base)).pow(length);

        System.out.println("For " + length + "-digit numbers of base " + base + ", expect a collision after "
                + trials + " trials (on average).  There are " + poss + " possibilities.");
    }
}
