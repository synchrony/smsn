########################################
# server and content

shinyServer(function(input, output, session) {

  output$foobar <- renderDataTable({
    entities.for.display(root.entities)
  }, escape=FALSE, options = list(pageLength=10))
  
	output$ui_line <- renderUI({
	  ## using renderUI here because Knitr will not create a slider
	  tagList(
	    sliderInput("nr_points", "", min = 10, max = 100, value = 50),
	    renderPlot({
	      nr <- if (is.null(input$nr_points)) 2 else input$nr_points
	      plot(1:nr, rnorm(nr))
	    })
	  )
	})

	sapply(list.files("pages"), function (f){
		output[[f]] <- renderUI({inclRmd(paste0("./pages/", f))})
	})
})

########################################
# SmSn-specific global variables and functions

find.increments <- function(v) {
  v[2:length(v)]-v[1:(length(v)-1)]
}

num <- function(x) { prettyNum(signif(x, 2), big.mark=",") }

showTable <- function(df, options=list()) {
  renderUI({
    tagList(renderDataTable(df, escape=FALSE, options=options))
  })
}

showFreqTable <- function(v, simple=TRUE) {
  df <- as.data.frame(table(v))
  df <- df[with(df, order(-Freq)),]
  tot <- sum(df$Freq)
  showTable(
    data.frame(
      value=df$v,
      count=sapply(df$Freq, num),
      "%"=sapply(df$Freq, function(n) { num(100 * n/tot)}), check.names=FALSE),
    if (simple) list(searching=FALSE, paging=FALSE) else list())
}

entities.for.display <- function(v) {
  title.cutoff <- 50
  titles.trunc <- sapply(v$title, function(s) {
    ch <- as.character(s)
    len <- nchar(ch)
    if (len > title.cutoff - 5)
      paste(substr(ch, 1, title.cutoff-5), "[...]", sep="")
    else ch
  })
  links = sapply(v$alias, function(s){
    alias <- as.character(s)
    if (nchar(alias) > 0) paste0("<a href='", alias, "'>link</a>") else ""
  })
  data.frame(
    id=v$id,
    source=v$source,
    created=ISOdatetime(1970,1,1,0,0,0, tz="GMT") + v$created/1000,
    weight=v$weight,
    title=titles.trunc,
    alias=links)
    # note: no @shortcut
}

entities <- read.table("data/vertices.tsv", header=TRUE, sep="\t", quote="", comment.char="")
total.entities <- nrow(entities)
unique.entities.id <- unique(entities$id)

relations <- read.table("data/edges.tsv", header=TRUE)
total.relations <- nrow(relations)

perc.of.entities <- function(n) {
  paste0("(", num(100 * n/total.entities), "% of total)")
}
