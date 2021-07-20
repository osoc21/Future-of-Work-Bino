library(shiny)
source("binomexfunction.r")


empty_mat = matrix(1,nrow = 3, ncol = 1)
curr_names = c("EUR","GBP","CAD")
empty_dat = cbind.data.frame(curr_names,empty_mat)
names(empty_dat) = c("Currency","Values")


ui <- fluidPage(
  titlePanel("Temporary title is temporary"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", NULL, accept = c(".csv", ".tsv")),
      rHandsontableOutput('contents'),
      tableOutput("text"),
      actionButton("go", "Plot Update"),
      numericInput("profsel", "row of profile to analyze", value = 1)
    ),
    
      
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("binplot")), 
        tabPanel("Summary", verbatimTextOutput("summary")), 
        tabPanel("Table", tableOutput("table"))
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
  
  selectedrow <- reactive(livedata()[input$profsel,])
  
  output$binplot <- renderPlot(
    binomexampleOSOC(MVT = selectedrow()$MVT, nfte = selectedrow()$FTE , jobtitle = selectedrow()$Profile),
    width = 900, height = 600
  )
  
}

shinyApp(ui, server)