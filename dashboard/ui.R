getdeps <- function() {
  htmltools::attachDependencies(
    htmltools::tagList(), c())
}

shinyUI(
  navbarPage("Semantic Synchrony dashboard",
    tabPanel("Structure", uiOutput("main")),
    tabPanel("Sources", uiOutput("sources")),
    tabPanel("Properties", uiOutput("properties")),
    tabPanel("Search", uiOutput("search"), getdeps())
  )
)
