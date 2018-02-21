getdeps <- function() {
  htmltools::attachDependencies(
    htmltools::tagList(), c())
}

shinyUI(
  navbarPage("Semantic Synchrony dashboard",
  #navlistPanel("Semantic Synchrony dashboard",
    tabPanel("Graph", uiOutput("main.Rmd")),
    tabPanel("Sources", uiOutput("sources.Rmd")),
    navbarMenu("Properties",
      tabPanel("@alias", uiOutput("alias.Rmd")),
      tabPanel("@created", uiOutput("created.Rmd")),
      tabPanel("@priority", uiOutput("priority.Rmd")),
      tabPanel("@shortcut", uiOutput("shortcut.Rmd")),
      tabPanel("@title", uiOutput("title.Rmd")),
      tabPanel("@weight", uiOutput("weight.Rmd"))),
  tabPanel("Activity", uiOutput("activity.Rmd")),
  tabPanel("Git", uiOutput("git.Rmd")),
  tabPanel("Search", uiOutput("search.Rmd"), getdeps())
  )
)
