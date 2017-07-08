library(shiny)

fluidPage(
  fluidRow(column(5, "Compare Book 1"),
           column(2),
           column(5, "...to Book 2")),
  fluidRow(
    column(2, 
           selectInput("search.type.lhs", label=NULL, 
                           choices=c("Title", "Author"), selected="Title")
           ),
    column(2, textInput("search.lhs", label = NULL, value="room with a view")),
    column(1, actionButton("go.lhs", label="Search")),
    column(2),
    column(2, 
           selectInput("search.type.rhs", label=NULL, 
                          choices=c("Title", "Author"), selected="Title")
          ),
    column(2, textInput("search.rhs", label = NULL, value="moby dick")),
    column(1,  actionButton("go.rhs", label="Search"))
  ),
  fluidRow(
    column(5, selectizeInput("options.lhs", label="Select one:", 
                             #selected=c(`A Room with a View (Forster, E. M. (Edward Morgan))`=2641),
                             choices=c(`A Room with a View (Forster, E. M. (Edward Morgan))`=2641), multiple=FALSE)),
    column(2),
    column(5, selectizeInput("options.rhs", label="Select one:", 
                             #selected=c(`Moby Dick; Or, The Whale (Melville, Herman)`=2701),
                             choices=c(`Moby Dick; Or, The Whale (Melville, Herman)`=2701), multiple=FALSE)
    )),
  fluidRow(
    verbatimTextOutput("comparison"),
    tableOutput("my.table")
  )
#   fluidRow(
#     column(5, tableOutput("lhs.table")),
#     column(2),
#     column(5, tableOutput(("rhs.table")))),
#   fluidRow(verbatimTextOutput("comparison"))
)