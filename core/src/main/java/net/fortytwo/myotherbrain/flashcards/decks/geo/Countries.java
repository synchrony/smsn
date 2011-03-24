package net.fortytwo.myotherbrain.flashcards.decks.geo;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/*
    See:
        http://www.dbis.informatik.uni-goettingen.de/Mondial/#RDF
        http://www.semanticoverflow.com/questions/3445/cities-and-countries-ontology

    wget http://www.dbis.informatik.uni-goettingen.de/Mondial/Mondial-RDF/mondial.rdf

    FINDING COUNTRY BORDERS ########

    rapper -i rdfxml -o ntriples mondial.rdf > mondial.nt
    grep bordering mondial.nt  > borders
    cat borders | sed 's/[_][:]//' | sed 's/[<].*countries.//' | sed 's/[/]...//'|sort > edges
    cat edges |sed 's/.*[ ]//' | tr '\n' ' '

    // Now split these into pairs like so:
    private static final String BORDERS = "...";
    public static void main(final String[] args) {
        String[] a = BORDERS.split(" ");
        for (int i = 0; i < a.length; i += 2) {
            System.out.println(a[i] + "\t" + a[i+1]);
        }
    }

    FINDING COUNTRY NAMES AND CAPITALS ##########

    roqet -r csv -i sparql -e "\
        SELECT DISTINCT ?countryCode ?countryName ?capitalCityName WHERE { \
            ?country <http://www.semwebtech.org/mondial/10/meta#name> ?countryName . \
            ?country <http://www.semwebtech.org/mondial/10/meta#carCode> ?countryCode . \
            OPTIONAL { \
                ?country <http://www.semwebtech.org/mondial/10/meta#capital> ?capital . \
                ?capital <http://www.semwebtech.org/mondial/10/meta#name> ?capitalCityName . \
            } \
        }" -D file:///data/tmp/mondial/mondial.rdf > tmp.txt
    cat tmp.txt | grep -v countryCode | sed 's/["][,]["]/_/' | sed 's/["][,]["]/_/' | sed 's/^.*[,]["]//' | sed 's/["]$//' | tr '_' '\t' | sed 's/["][,]//' | sort > countries.txt

    FINDING US STATE NAMES AND CAPITALS #########

    roqet -r csv -i sparql -e "\
        SELECT DISTINCT ?stateCode ?stateName ?capitalCityName WHERE { \
            ?country <http://www.semwebtech.org/mondial/10/meta#name> ?stateName . \
            ?country <http://www.semwebtech.org/mondial/10/meta#carCode> ?stateCode . \
            OPTIONAL { \
                ?country <http://www.semwebtech.org/mondial/10/meta#capital> ?capital . \
                ?capital <http://www.semwebtech.org/mondial/10/meta#name> ?capitalCityName . \
            } \
        }" -D file:///data/tmp/mondial/mondial.rdf > tmp.txt
    cat tmp.txt | grep -v countryCode | sed 's/["][,]["]/_/' | sed 's/["][,]["]/_/' | sed 's/^.*[,]["]//' | sed 's/["]$//' | tr '_' '\t' | sed 's/["][,]//' | sort > countries.txt

*/
public class Countries {
    public class Country {
        public String code;
        public String name;
        public City capitalCity;
        public List<Country> neighbors;
    }

    public class City {
        public String name;
    }

    private final Map<String, Country> countriesByCode;

    private static final Countries INSTANCE;

    static {
        try {
            INSTANCE = new Countries();
        } catch (IOException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public static Countries getInstance() {
        return INSTANCE;
    }

    private Countries() throws IOException {
        countriesByCode = new HashMap<String, Country>();

        InputStream is = Countries.class.getResourceAsStream("countries.txt");
        try {
            BufferedReader br = new BufferedReader(new InputStreamReader(is));
            String l;
            while ((l = br.readLine()) != null) {
                l = l.trim();
                if (0 < l.length()) {
                    String[] a = l.split("\\t");
                    // Note: there are no duplicate countries in the file:
                    //     wc -l countries.txt
                    //     cat countries.txt | tr '\t' '_'|sed 's/_.*//'|sort -u|wc -l
                    Country c = new Country();
                    c.code = a[0].trim();
                    c.name = a[1].trim();
                    c.neighbors = new LinkedList<Country>();

                    // Not all countries have a capital city in this data set (e.g. GAZA, WEST)
                    if (a.length > 2) {
                        City t = new City();
                        t.name = a[2].trim();
                        c.capitalCity = t;
                    }

                    countriesByCode.put(c.code, c);
                }
            }
        } finally {
            is.close();
        }

        is = Countries.class.getResourceAsStream("international_borders.txt");
        try {
            BufferedReader br = new BufferedReader(new InputStreamReader(is));
            String l;
            while ((l = br.readLine()) != null) {
                l = l.trim();
                if (0 < l.length()) {
                    int i = l.indexOf('\t');

                    // Note: there are no duplicate countries in the file
                    String tailCode = l.substring(0, i).trim();
                    String headCode = l.substring(i + 1).trim();
                    Country tail = countriesByCode.get(tailCode);
                    Country head = countriesByCode.get(headCode);

                    if (null == tail || null == head) {
                        if (null == tail) {
                            System.err.println("no such country: " + tailCode);
                        }
                        if (null == head) {
                            System.err.println("no such country: " + headCode);
                        }
                    } else {
                        tail.neighbors.add(head);
                    }
                }
            }
        } finally {
            is.close();
        }

        // Alphabetize lists of neighboring countries.
        for (Country c : countriesByCode.values()) {
            Collections.sort(c.neighbors, new Comparator<Country>() {
                public int compare(final Country c1,
                                   final Country c2) {
                    return c1.name.compareTo(c2.name);
                }
            });
        }
    }

    public Collection<Country> getCountries() {
        return countriesByCode.values();
    }

    public static void main(final String[] args) {
    }
}
