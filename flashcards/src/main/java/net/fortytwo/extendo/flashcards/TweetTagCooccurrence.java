package net.fortytwo.extendo.flashcards;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * User: josh
 * Date: 3/24/11
 * Time: 6:20 PM
 */
public class TweetTagCooccurrence {
    public static void main(final String[] args) throws IOException {
        Map<String, List<String>> taggings = new HashMap<String, List<String>>();
        Map<String, Integer> weight = new HashMap<String, Integer>();
        Set<String> allTags = new HashSet<String>();
        InputStream is = new FileInputStream("/Users/josh/data/casia/tweets/tweet_topics.tab");
        try {
            InputStreamReader r = new InputStreamReader(is, "UTF-8");
            BufferedReader br = new BufferedReader(r);
            String l;
            while ((l = br.readLine()) != null) {
                //System.out.println("l = " + l);
                int i = l.indexOf('\t');
                String tweet = l.substring(0, i);
                String tag = l.substring(i + 1);
                List<String> t = taggings.get(tweet);
                if (null == t) {
                    t = new LinkedList<String>();
                    taggings.put(tweet, t);
                }
                t.add(tag);
            }
        } finally {
            is.close();
        }

        //System.out.println("" + taggings.keySet().size() + " taggings ");
        //System.exit(1);
        for (List<String> l : taggings.values()) {
            for (int i = 0; i < l.size(); i++) {
                String tag1 = l.get(i);
                if (l.size() > 1) {
                    allTags.add(tag1);
                }
                for (int j = i + 1; j < l.size(); j++) {
                    String tag2 = l.get(j);
                    allTags.add(tag2);
                    String label = findLabel(tag1, tag2);
                    //System.out.println("label = " + label);
                    Integer n = weight.get(label);
                    if (null == n) {
                        n = 0;
                    }
                    n++;
                    weight.put(label, n);
                    //System.out.println("\t" + n);
                }
            }
        }

        List<String> ordered = new LinkedList<String>();
        ordered.addAll(allTags);
        Collections.sort(ordered);
        Map<String, Integer> reverse = new HashMap<String, Integer>();
        for (int i = 0; i < ordered.size(); i++) {
            reverse.put(ordered.get(i), i);
        }

        OutputStream os = new FileOutputStream("/tmp/tweet_tags.txt");
        PrintStream ps = new PrintStream(os);
        try {
            for (String tag : ordered) {
                ps.println(tag);
            }
            /*
          for (String label : weight.keySet()) {
              int i = label.indexOf(" ");
              String tag1 = label.substring(0, i);
              String tag2 = label.substring(i + 1);
              int w = weight.get(label);
              ps.println(tag1 + "\t" + tag2 + "\t" + w);
          }  */
        } finally {
            os.close();
        }

        //os = new FileOutputStream("/tmp/tweet_tag_cooccurrence.txt");
        os = new FileOutputStream("/tmp/tweet_tag_cooccurrence.ncol");
        ps = new PrintStream(os);
        try {
            for (String label : weight.keySet()) {
                int i = label.indexOf(" ");
                String tag1 = label.substring(0, i);
                String tag2 = label.substring(i + 1);
                int w = weight.get(label);
                //ps.println("" + reverse.get(tag1) + "\t" + reverse.get(tag2) + "\t" + w);
                ps.println("" + tag1 + "\t" + tag2 + "\t" + w);
            }
            /*
            for (String tag1 : ordered) {
                StringBuilder sb = new StringBuilder();
                boolean first = true;
                for (String tag2 : ordered) {
                    String label = tag1 + " " + tag2;
                    if (first) {
                        first = false;
                    }             else {
                        sb.append("\t");
                    }
                    Integer w = weight.get(label);
                    sb.append(null == w ? 0 : w);
                }
                ps.println(sb.toString());
            }  */
            /*
          for (String label : weight.keySet()) {
              int i = label.indexOf(" ");
              String tag1 = label.substring(0, i);
              String tag2 = label.substring(i + 1);
              int w = weight.get(label);
              ps.println(tag1 + "\t" + tag2 + "\t" + w);
          }  */
        } finally {
            os.close();
        }
    }

    private static String findLabel(final String tag1,
                                    final String tag2) {
        return tag1.compareTo(tag2) < 0
                ? tag1 + " " + tag2
                : tag2 + " " + tag1;
    }
}
