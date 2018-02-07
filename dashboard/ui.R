getdeps <- function() {
  htmltools::attachDependencies(
    htmltools::tagList(), c())
}

shinyUI(
  navbarPage("Semantic Synchrony dashboard",
    tabPanel("Structure", uiOutput("main")),
    tabPanel("Sources", uiOutput("sources")),
    navbarMenu("Properties",
               tabPanel("@weight", uiOutput("weight")),
               tabPanel("@title", uiOutput("title")),
               tabPanel("@alias", uiOutput("alias")),
               tabPanel("@priority", uiOutput("priority")),
               tabPanel("@shortcut", uiOutput("shortcut"))),
    tabPanel("Search", uiOutput("search"), getdeps())
  )
)
