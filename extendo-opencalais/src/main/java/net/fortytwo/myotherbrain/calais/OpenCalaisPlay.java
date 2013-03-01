package net.fortytwo.myotherbrain.calais;

import mx.bigdata.jcalais.CalaisClient;
import mx.bigdata.jcalais.CalaisConfig;
import mx.bigdata.jcalais.CalaisObject;
import mx.bigdata.jcalais.CalaisResponse;
import mx.bigdata.jcalais.rest.CalaisRestClient;
import net.fortytwo.extendo.ExtendoBrain;
import net.fortytwo.extendo.ExtendoBrain;

import java.io.FileInputStream;
import java.net.URL;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class OpenCalaisPlay {
    public static final String API_KEY = "net.fortytwo.myotherbrain.calais.apiKey";
    
    public static void main(final String[] args) {
        try {
            play();
        } catch (Throwable e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private static void play() throws Exception {
        ExtendoBrain.getConfiguration().load(new FileInputStream("/tmp/open-calais-play.props"));

        String apiKey = ExtendoBrain.getConfiguration().getString(API_KEY);
        System.out.println("key: " + apiKey);

        CalaisClient client = new CalaisRestClient(apiKey);

        CalaisConfig config = new CalaisConfig();
        config.set(CalaisConfig.ProcessingParam.CALCULATE_RELEVANCE_SCORE, "true");
        config.set(CalaisConfig.ProcessingParam.DOC_RDFACCESSIBLE, "true");
        config.set(CalaisConfig.UserParam.ALLOW_DISTRIBUTION, "true");
        config.set(CalaisConfig.UserParam.ALLOW_SEARCH, "true");
        //config.set(CalaisConfig.UserParam.EXTERNAL_ID, "User generated ID");

//        CalaisResponse response = client.analyze(new URL("http://twitlogic.fortytwo.net"));
        CalaisResponse response = client.analyze(new URL("http://ripple.fortytwo.net"));
//        CalaisResponse response = client.analyze(new URL("http://blog.fortytwo.net"));
//        CalaisResponse response = client.analyze(new URL("http://en.wikipedia.org/wiki/Supply_chain"));

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
    }
}
