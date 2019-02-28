library(shiny)
library(markdown)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("flatly"),
                
  tags$head(tags$title("NATS"),
            tags$link(rel = "stylesheet", type = "text/css", href = "www/bootstrap.css")),
  
  navbarPage("Time Series Analysis Software",
    tabPanel("Start",
      navlistPanel(widths=c(2,10),
        tabPanel("What does NATS do?",
          wellPanel(includeHTML("www/nats_do.html"))),
        tabPanel("What does NATS need?",
          wellPanel(includeHTML("www/beanz_need.html"))),
        tabPanel("What does NATS provide?",
          wellPanel(includeHTML("www/beanz_need.html"))),
        tabPanel("Warnings",
          wellPanel(includeHTML("www/beanz_need.html")))
        )
    ),
    
    tabPanel("Upload Data",
             fluidPage(
               wellPanel(
                 h4("Upload data"),
                 fluidRow(
                   column(3,
                          h5(strong("Choose File")),
                          fileInput(inputId = 'userdata', label = '',
                                    accept=c('text/csv','text/comma-separated-values,text/plain'))),
                   column(2, h5(strong("Separator")),
                          radioButtons('sep', '',
                                       c(Comma=',',Semicolon=';',Tab='\t',Space=' '),
                                       ' ')),
                   column(2, h5(strong("Quote")),
                          radioButtons('quote', '',
                                       c(None='','Double Quote'='"','Single Quote'="'"),
                                       selected = '')),
                   column(2, h5(strong("Other")),
                          checkboxInput(inputId='header', label='Header', value=TRUE),
                          checkboxInput(inputId="displaydata", label = "Show Data", value = TRUE))
                 )),
               wellPanel(
                 h4("Try An Example"),
                 includeHTML("www/beanz_solvd.html"),
                 actionButton("btnExample", "Try it", styleclass = "success")
               ),
               wellPanel(h4("Review data"), dataTableOutput("uiData"))
             )
    ),
    tabPanel("Probability Distribution", 
             verbatimTextOutput("summary")
    ),
    tabPanel("Data Visualization", 
             verbatimTextOutput("summary")
    ),
    tabPanel("Descriptors", 
             verbatimTextOutput("summary")
    )
 )
)


server <- function(input, output, session) {
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })
  
  output$summary <- renderPrint({
    summary(cars)
  })
}

shinyApp(ui = ui, server = server)