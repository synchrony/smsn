package net.fortytwo.extendo.brain.util;

import net.fortytwo.extendo.brain.NoteQueries;

import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

/**
 * Note: the LCS algorithm is probably not optimal in the sense of minimizing vertex and edge churn
 * e.g. ab --> ba destroys one vertex and one edge, and creates one vertex and one edge.
 * A better algorithm would use a swap operation, merely deleting one edge and creating one edge.
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ListDiff {

    public static List<String> toList(final String s) {
        List<String> result = new LinkedList<String>();

        for (byte b : s.getBytes()) {
            result.add("" + (char) b);
        }

        return result;
    }

    public static String toString(List<String> l) {
        StringBuilder sb = new StringBuilder();
        for (String s : l) {
            sb.append(s);
        }

        return sb.toString();
    }

    public static <T> List<T> leastCommonSubsequence(List<T> a, List<T> b, Comparator<T> comp) {
        int[][] lengths = new int[a.size() + 1][b.size() + 1];

        // row 0 and column 0 are initialized to 0 already

        for (int i = 0; i < a.size(); i++)
            for (int j = 0; j < b.size(); j++)
                if (0 == comp.compare(a.get(i), b.get(j)))
                    lengths[i + 1][j + 1] = lengths[i][j] + 1;
                else
                    lengths[i + 1][j + 1] =
                            Math.max(lengths[i + 1][j], lengths[i][j + 1]);

        // read the substring out from the matrix
        List<T> result = new LinkedList<T>();
        for (int x = a.size(), y = b.size();
             x != 0 && y != 0; ) {
            if (lengths[x][y] == lengths[x - 1][y])
                x--;
            else if (lengths[x][y] == lengths[x][y - 1])
                y--;
            else {
                assert 0 == comp.compare(a.get(x - 1), b.get(y - 1));
                result.add(a.get(x - 1));
                x--;
                y--;
            }
        }

        Collections.reverse(result);

        return result;
    }

    public static class StringComparator implements Comparator<String> {
        public int compare(String a, String b) {
            return a.compareTo(b);
        }
    }

    public static <T> void applyDiff(final List<T> a,
                                     final List<T> b,
                                     final List<T> l,
                                     final Comparator<T> cmp,
                                     final DiffEditor<T> ed) throws NoteQueries.InvalidUpdateException {
        int acur = 0;
        int bcur = 0;
        int lcur = 0;
        int ecur = 0;

        while (lcur < l.size()) {
            if (0 != cmp.compare(l.get(lcur), a.get(acur))) {         // absent in the subsequence but present in the original
                ed.delete(ecur, a.get(acur));
                acur++;
            } else if (0 != cmp.compare(l.get(lcur), b.get(bcur))) {  // absent in the subsequence but present in the second sequence
                ed.add(ecur, b.get(bcur));
                bcur++;
                ecur++;
            } else {
                lcur++;
                acur++;
                bcur++;
                ecur++;
            }
        }

        int aleftover = a.size() - acur;
        int bleftover = b.size() - bcur;

        for (int i = 0; i < aleftover; i++) {
            ed.delete(ecur, a.get(acur));
            acur++;
        }
        for (int i = 0; i < bleftover; i++) {
            ed.add(ecur, b.get(bcur));
            bcur++;
            ecur++;
        }
    }

    public static interface DiffEditor<T> {
        void add(int position, T letter) throws NoteQueries.InvalidUpdateException;

        void delete(int position, T letter);
    }
}
