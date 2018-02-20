# Semantic Synchrony dashboard

This directory contains an RStudio project that provides a dashboard-style user interface for a SmSn knowledge graph.
Here is how you can use the dashboard:

* download and install [RStudio](https://www.rstudio.com/), then choose "File > Open project..." and navigate to the current directory.
* in SmSn, export vertices with `C-c C-w v` and export edges with `C-c C-w e`.
* create a directory called `data` under the current directory, and copy the vertex and edge files as `./data/vertices.tsv` and `./data/edges.tsv`.
* open ui.R in RStudio and select "Run App"

If you have any trouble with this process, kindly raise an issue on the [issue tracker](https://github.com/synchrony/smsn/issues).
