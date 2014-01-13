package net.fortytwo.extendo.calais;

import mx.bigdata.jcalais.CalaisClient;
import mx.bigdata.jcalais.CalaisConfig;
import mx.bigdata.jcalais.CalaisObject;
import mx.bigdata.jcalais.CalaisResponse;
import mx.bigdata.jcalais.rest.CalaisRestClient;
import net.fortytwo.extendo.Extendo;

import java.io.FileInputStream;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class OpenCalaisPlay {
    public static final String API_KEY = "net.fortytwo.extendo.calais.apiKey";
    
    public static void main(final String[] args) {
        try {
            play();
        } catch (Throwable e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private static void play() throws Exception {
        Extendo.getConfiguration().load(new FileInputStream("/tmp/opencalais-play.props"));

        String apiKey = Extendo.getConfiguration().getString(API_KEY);
        System.out.println("key: " + apiKey);

        CalaisClient client = new CalaisRestClient(apiKey);

        CalaisConfig config = new CalaisConfig();
        config.set(CalaisConfig.ProcessingParam.CALCULATE_RELEVANCE_SCORE, "true");
        config.set(CalaisConfig.ProcessingParam.DOC_RDFACCESSIBLE, "true");
        config.set(CalaisConfig.UserParam.ALLOW_DISTRIBUTION, "true");
        config.set(CalaisConfig.UserParam.ALLOW_SEARCH, "true");
        //config.set(CalaisConfig.UserParam.EXTERNAL_ID, "User generated ID");

//        CalaisResponse response = client.analyze(new URL("http://twitlogic.fortytwo.net"));
//        CalaisResponse response = client.analyze(new URL("http://ripple.fortytwo.net"));
//        CalaisResponse response = client.analyze(new URL("http://blog.fortytwo.net"));
//        CalaisResponse response = client.analyze(new URL("http://en.wikipedia.org/wiki/Supply_chain"));

        //CalaisResponse response = client.analyze("Diabetes mellitus, or simply diabetes, is a group of metabolic diseases in which a person has high blood sugar, either because the pancreas does not produce enough insulin, or because cells do not respond to the insulin that is produced");

        long before = System.currentTimeMillis();
        CalaisResponse response = client.analyze("The common cause of all forms of otitis media is blockage of the Eustachian tube. This is usually due to swelling of the mucous membranes in the nasopharynx, which in turn can be caused by a viral upper respiratory infection or by allergies.");
        long after = System.currentTimeMillis();

        System.out.println("recognized entities:");
        for (CalaisObject entity : response.getEntities()) {
            System.out.println("\t" + entity.getField("_type") + ":"
                    + entity.getField("name"));
            for (String n : entity.getFieldNames()) {
                System.out.println("\t\t" + n + ":\t" + entity.getField(n));
            }
        }

        System.out.println("topics:");
        for (CalaisObject topic : response.getTopics()) {
            System.out.println("\t" + topic.getField("categoryName"));
        }

        System.out.println("social tags:");
        for (CalaisObject tags : response.getSocialTags()){
            System.out.println("\t" + tags.getField("_typeGroup") + ":"
                    + tags.getField("name"));
        }

        long allDone = System.currentTimeMillis();

        System.out.println("finished in " + (after - before) + "ms (" + (allDone - before) + "ms incuding output)");
    }
}
