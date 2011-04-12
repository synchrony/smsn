package net.fortytwo.myotherbrain.flashcards.decks.tech;

import net.fortytwo.myotherbrain.flashcards.Card;
import net.fortytwo.myotherbrain.flashcards.Deck;
import net.fortytwo.myotherbrain.flashcards.db.CardStore;
import net.fortytwo.myotherbrain.flashcards.db.CloseableIterator;
import net.fortytwo.myotherbrain.flashcards.decks.Answer;
import net.fortytwo.myotherbrain.flashcards.decks.AnswerFormatter;
import net.fortytwo.myotherbrain.flashcards.decks.InformationSource;
import net.fortytwo.myotherbrain.flashcards.decks.QuestionFormatter;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

/**
 * User: josh
 * Date: 4/3/11
 * Time: 12:33 PM
 */
public class HttpStatusCodes extends Deck<String, String> {
    /*
    wget http://www.w3.org/2008/http-statusCodes
    rapper -i rdfxml -o ntriples http-statusCodes > /tmp/http-statusCodes.nt

    roqet -r csv -i sparql -e "\
        PREFIX http: <http://www.w3.org/2006/http#> \
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \
        PREFIX dc: <http://purl.org/dc/elements/1.1/> \
        SELECT DISTINCT ?code ?title WHERE { \
            ?code rdf:type http:StatusCode . \
            ?code dc:title ?title . \
        }" -D file:///tmp/http-statusCodes.nt > tmp.csv

    cat tmp.csv | sed 's/.*statusCode//'| sed 's/[)],"/_/' | sed 's/".*$//' | tr '_' '\t' | sort
    */

    private static final String
            WIKIPEDIA_PREFIX = "http://en.wikipedia.org/wiki/HTTP_";

    private final CardStore<String, String> store;
    private final InformationSource source;

    public HttpStatusCodes(final Format format,
                           final CardStore<String, String> store) throws IOException {
        super("http_status_codes", "HTTP status codes");
        this.store = store;

        source = new InformationSource("HTTP Vocabulary");
        source.setUrl("http://www.w3.org/TR/HTTP-in-RDF10/");
        source.setTimestamp("Tue Apr 12 11:17:38 CST 2011");

        InputStream is = HttpStatusCodes.class.getResourceAsStream("http_status_codes.txt");
        try {
            BufferedReader br = new BufferedReader(new InputStreamReader(is));
            String l;
            while ((l = br.readLine()) != null) {
                l = l.trim();
                if (0 < l.length()) {
                    String a[] = l.split("\\t");
                    final String code = a[0];
                    final String label = a[1];
                    final String url = WIKIPEDIA_PREFIX + code;

                    Card<String, String> card = new Card<String, String>(code, this) {
                        @Override
                        public String getQuestion() {
                            StringBuilder sb = new StringBuilder();
                            sb.append(code);
                            sb.append(" = ?");

                            QuestionFormatter f = new QuestionFormatter(deck, format);
                            f.setQuestion(sb.toString());
                            return f.format();
                        }

                        @Override
                        public String getAnswer() {
                            AnswerFormatter f = new AnswerFormatter(format);
                            Answer a = new Answer();
                            f.addAnswer(a);
                            a.setSource(source);
                            a.addForm("" + code, url);
                            a.setMeaning(label);
                            return f.format();
                        }
                    };

                    store.add(card);
                }
            }
        } finally {
            is.close();
        }
    }

    @Override
    public Card<String, String> getCard(final String name) {
        return store.find(this, name);
    }

    @Override
    public CloseableIterator<Card<String, String>> getCards() {
        return store.findAll(this);
    }
}
