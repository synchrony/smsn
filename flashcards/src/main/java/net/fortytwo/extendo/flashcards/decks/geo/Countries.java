package net.fortytwo.extendo.flashcards.decks.geo;

import net.fortytwo.extendo.flashcards.decks.InformationSource;

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

    roqet -r csv -i sparql -e "\
        PREFIX mondial: <http://www.semwebtech.org/mondial/10/meta#> \
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \
        SELECT DISTINCT ?country1 ?country2 WHERE { \
            ?b mondial:bordering ?country1 . \
            ?b mondial:bordering ?country2 . \
            ?country1 rdf:type mondial:Country . \
            ?country2 rdf:type mondial:Country . \
            FILTER (?country1 != ?country2) . \
        }" -D file:///data/tmp/mondial/mondial.rdf > tmp.txt
    cat tmp.txt | sed 's/countries/_./' | sed 's/.*_..//' | sed 's/[/].*countries./_/'|sed 's/[/].*$//'| tr '_' '\t'|sort

    FINDING COUNTRY NAMES AND CAPITALS ##########

    roqet -r csv -i sparql -e "\
        PREFIX mondial: <http://www.semwebtech.org/mondial/10/meta#> \
        SELECT DISTINCT ?countryCode ?countryName ?area ?pop ?gdp ?capitalCityName WHERE { \
            ?country mondial:name ?countryName . \
            ?country mondial:carCode ?countryCode . \
            OPTIONAL { ?country mondial:area ?area . } \
            OPTIONAL { ?country mondial:population ?pop . } \
            OPTIONAL { ?country mondial:gdpTotal ?gdp . } \
            OPTIONAL { \
                ?country mondial:capital ?capital . \
                ?capital mondial:name ?capitalCityName . \
            } \
        }" -D file:///data/tmp/mondial/mondial.rdf > tmp.txt
        cat tmp.txt | grep -v countryCode | sed 's/["][,]["]/_/' | sed 's/["][,]/_/' | sed 's/,/_/' | sed 's/,/_/' | sed 's/,/_/' | sed 's/,["]/_/' | sed 's/["]$//' | sed 's/^.*["]//' | sed 's/[,]$//' | tr '_' '\t' | sort > countries.txt

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
        public Double areaInSqKm;
        public Integer population;
        public Double gdp;
        public City capitalCity;
        public List<Country> neighbors;
    }

    public class City {
        public String name;
    }

    private final Map<String, Country> countriesByCode;
    private final InformationSource source;

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
            int line = 0;
            while ((l = br.readLine()) != null) {
                line++;
                try {
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

                        c.areaInSqKm = 0 == a[2].length() ? null : Double.valueOf(a[2]);
                        c.population = 0 == a[3].length() ? null : Integer.valueOf(a[3]);
                        c.gdp = 0 == a[4].length() ? null : Double.valueOf(a[4]);

                        // Not all countries have a capital city in this data set (currently GAZA, WEST)
                        if (a.length > 5) {
                            City t = new City();
                            t.name = a[5].trim();
                            c.capitalCity = t;
                        }

                        countriesByCode.put(c.code, c);
                    }
                } catch (Exception e) {
                    throw new IOException("error on line " + line, e);
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

        // Note: the Mondial dataset appears to be very significantly out of date w.r.t. population and (to an even
        // greater extent: often a factor of 2), GDP
        source = new InformationSource("Mondial in RDF");
        source.setUrl("http://www.dbis.informatik.uni-goettingen.de/Mondial/#RDF");
        source.setTimestamp("Tue Apr 12 11:17:38 CST 2011");
    }

    public Collection<Country> getCountries() {
        return countriesByCode.values();
    }

    public InformationSource getSource() {
        return source;
    }

    public static void main(final String[] args) {
    }
}
