library(shiny)
source("binomexfunction.r")
library(rhandsontable)

empty_dat = matrix(nrow = 1,ncol = 5)
colnames(empty_dat) = c("Profile","FTE","MVT","Pattr","RT")


ui <- fluidPage(
  titlePanel("Temporary title is temporary"),
  
  sidebarLayout(
    sidebarPanel(
      

      fileInput("file1", NULL, accept = c(".csv", ".tsv")),
      fileInput("file2", NULL, accept = c('.csv','.tsv')),
      rHandsontableOutput('contents'),
      actionButton("go", "Plot Update"),
      numericInput("profsel", "row of profile to analyze", value = 0, min = 0, max = 0)
      ),
    
      
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("binplot")), 
        tabPanel("Summary", verbatimTextOutput("summary")), 
        tabPanel("Table", tableOutput("table")),
        tabPanel("text3", textOutput("text3"))
    )
  )
)
)

server <- function(input, output, session) {
  
  indat <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(rhandsontable(empty_dat))
    raw_input = read.csv(inFile$datapath, header=T, sep = ";")
    return(rhandsontable(raw_input))
  })
  
  output$contents <- renderRHandsontable({
    indat()
  })
  
  livedata <- eventReactive(input$go, {
    live_data = hot_to_r(input$contents)
    return(live_data)
  })
  
  observe({
    updateNumericInput(inputId = "profsel", min = 1, max = nrow(livedata()), value = 1)
  })
  
  selectedrow <- reactive(livedata()[input$profsel,])
  
  output$binplot <- renderPlot(
    binomexampleOSOC(MVT = selectedrow()$MVT, nfte = selectedrow()$FTE , jobtitle = selectedrow()$Profile),
    width = 900, height = 600
  )
  
}

shinyApp(ui, server)