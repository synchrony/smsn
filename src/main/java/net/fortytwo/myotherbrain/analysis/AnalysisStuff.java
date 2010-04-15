package net.fortytwo.myotherbrain.analysis;


/**
 * Author: josh
 * Date: May 5, 2009
 * Time: 6:34:26 PM
 */
public class AnalysisStuff {
    /*
PageRank computed on a power-law distribution with 0 edges over 0 nodes in 0 ms
PageRank computed on a power-law distribution with 500000 edges over 500000 nodes in 2032 ms
PageRank computed on a power-law distribution with 1000000 edges over 1000000 nodes in 3961 ms
PageRank computed on a power-law distribution with 1500000 edges over 1500000 nodes in 6138 ms
PageRank computed on a power-law distribution with 2000000 edges over 2000000 nodes in 15343 ms
PageRank computed on a power-law distribution with 2500000 edges over 2500000 nodes in 10352 ms
PageRank computed on a power-law distribution with 3000000 edges over 3000000 nodes in 12250 ms
PageRank computed on a power-law distribution with 3500000 edges over 3500000 nodes in 64962 ms
     */       /*
    public static void main(final String[] args) {
        AnalysisStuff t = new AnalysisStuff();

        for (int i = 0; i <= 8000000; i += 500000) {
            t.testPageRankSpeedOnPowerLawDistribution(i);
        }
    }

    private void testPageRankSpeedOnPowerLawDistribution(final int size) {
        Graph<Integer, Integer> graph = new RandomKnowledgeBaseFactory(size).create();
        double dampingFactor = 0.85;

        PageRank<Integer, Integer> ranking
                = new PageRank<Integer, Integer>(graph, 1.0 - dampingFactor);

        long before = System.currentTimeMillis();
        ranking.evaluate();
        long after = System.currentTimeMillis();
        System.out.println("PageRank computed on a power-law distribution with "
                + graph.getEdgeCount() + " edges over "
                + graph.getVertexCount()
                + " nodes in " + (after - before) + " ms");
    }

    private class RandomKnowledgeBaseFactory implements Factory<Graph<Integer, Integer>> {
        private int nextNodeID;
        private final int size;

        public RandomKnowledgeBaseFactory(final int size) {
            this.size = size;
        }

        public Graph<Integer, Integer> create() {
            nextNodeID = 0;

            Factory<Graph<Integer, Integer>> graphFactory
                    = new Factory<Graph<Integer, Integer>>() {

                public Graph<Integer, Integer> create() {
                    return new DirectedSparseGraph<Integer, Integer>();
                }
            };
            Factory<Integer> vertexFactory = new Factory<Integer>() {
                public Integer create() {
                    return ++nextNodeID;
                }
            };
            Factory<Integer> edgeFactory = new Factory<Integer>() {
                public Integer create() {
                    return ++nextNodeID;
                }
            };

            int numberOfVertices = size;
            int numberOfEdges = size;
            int iterations = (size < 1000) ? size : 1000;

            GraphGenerator<Integer, Integer> gen
                    = new EppsteinPowerLawGenerator<Integer, Integer>(
                    graphFactory, vertexFactory, edgeFactory,
                    numberOfVertices, numberOfEdges, iterations);

            return gen.create();
        }
    } */
}
