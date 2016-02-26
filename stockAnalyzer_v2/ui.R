library(shiny)

shinyUI(fluidPage(
  
  titlePanel(NULL),
  
  fluidRow(
    column(3, wellPanel(
      selectizeInput("select.in", label="Select One or More Tickers:", selected=c("GM","HMC"),
                     choices=symbols, multiple=T),
      
      dateRangeInput('date.range',
                     label = paste('Date Range:'),
                     start = Sys.Date() - 120, end = Sys.Date(),
                     min = Sys.Date() - 3650, max = Sys.Date(),
                     separator = " - ", format = "dd/mm/yy",
                     startview = 'year', language = 'en', weekstart = 1
      ),
      
      submitButton("Submit")
    )),

    column(9,
           plotOutput("plot") #,
           #dataTableOutput("table")
    )
  )
))
