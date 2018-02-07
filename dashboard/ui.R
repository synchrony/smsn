getdeps <- function() {
  htmltools::attachDependencies(
    htmltools::tagList(), c())
}

shinyUI(
  navbarPage("Semantic Synchrony dashboard",
  #navlistPanel("Semantic Synchrony dashboard",
    tabPanel("Structure", uiOutput("main")),
    tabPanel("Sources", uiOutput("sources")),
    navbarMenu("Properties",
      tabPanel("@alias", uiOutput("alias")),
      tabPanel("@created", uiOutput("created")),
      tabPanel("@priority", uiOutput("priority")),
      tabPanel("@shortcut", uiOutput("shortcut")),
      tabPanel("@title", uiOutput("title")),
      tabPanel("@weight", uiOutput("weight"))),
  tabPanel("Search", uiOutput("search"), getdeps())
  )
)
