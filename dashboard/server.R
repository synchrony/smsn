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

# get the global environment
env <- environment()

find.increments <- function(v) {
  v[2:length(v)]-v[1:(length(v)-1)]
}

num <- function(x) { prettyNum(signif(x, 2), big.mark=",") }

showTable <- function(df, options=list()) {
  renderUI({
    tagList(renderDataTable(df, escape=FALSE, options=options))
  })
}

showSimpleTable <- function(df) {
  showTable(df, list(searching=FALSE, paging=FALSE))
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

require.entities <- function() {
  if (is.null(env$entities)) {
    f <- "data/vertices.tsv"
    if (!file.exists(f)) {
      stop(paste0("entity file does not exist: ", f))
    }

    env$entities <- read.table(f, header=TRUE, sep="\t", quote="", comment.char="")
    env$total.entities <- nrow(entities)
    env$unique.entities.id <- unique(entities$id)
  }
}

require.relations <- function() {
  if (is.null(env$relations)) {
    f <- "data/edges.tsv"
    if (!file.exists(f)) {
      stop(paste0("relation file does not exist: ", f))
    }
    
    env$relations <- read.table(f, header=TRUE)
    env$total.relations <- nrow(relations)
    env$entities.and.relations <- TRUE
  }
}

require.entities.and.relations <- function() {
  require.entities()
  require.relations()
}

hour <- 60*60*1000   # milliseconds per hour
day <- 24*hour       # milliseconds per day

to.time <- function(unix.ms) {
  as.POSIXct(as.numeric(as.vector(unix.ms))/1000, origin="1970-01-01 GMT", tz="GMT")
}

# Note: occasionally, activity.log becomes corrupted (due to a killed process)
# and needs to be manually cleaned up (by deleting an invalid line).
require.activity.log <- function() {
  if (is.null(env$activity)) {
    f <- "data/activity.log"
    if (!file.exists(f)) {
      stop(paste0("activity log does not exist: ", f))
    }
    
    activity.data <- read.table(f, header=FALSE, sep="\t", fill=TRUE, col.names=c("time.raw", "action", "entity1", "entity2"))
    env$time <- to.time(activity.data$time.raw)
    activity.data <- subset(activity.data, !is.na(time))
    activity <- data.frame(activity.data, time=to.time(activity.data$time.raw))
    env$activity <- activity[with(activity, order(time)),]
  }
}

require.git.history <- function() {
  if (is.null(env$git)) {
    f <- "data/git-log.csv"
    if (!file.exists(f)) {
      stop(paste0("git history does not exist: ", f))
    }
    
    env$git <- read.csv(f, header=FALSE)
    env$git.log.file <- f
  }
}

perc.of.entities <- function(n) {
  paste0("(", num(100 * n/total.entities), "% of total)")
}

find.sources <- function() {
  unique(as.vector(env$entities$source))
}

