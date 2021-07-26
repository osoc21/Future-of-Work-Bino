### current issues:
# - everything
# - hacky way of doing rowselect :: somehow not same order in dump()$Profile objects as in live_data_infomat()$Profile, really annoying

### current todo:
# - make code readable
# - fix layouting
# - add shit
# - fix/implement plot adding again
# - add readability features to app thing (ugh)



library(shiny)
source("binomexfunction.r")
library(rhandsontable)

empty_dat = matrix(nrow = 1,ncol = 5)
colnames(empty_dat) = c("Profile","FTE","MVT","Pattr","RT")


empty_dat2 = matrix(nrow = 1,ncol = 10)
colnames(empty_dat2) = c("Profile",paste0('Y',rep(0:8)))

dump <- list()



nruns = 1000
timescale = 8

ui <- fluidPage(
  titlePanel("Temporary title is temporary"),
  
  sidebarLayout(
      
      fileInput("file1", NULL, accept = c(".csv")),
      fileInput("file2", NULL, accept = c('.csv')),
      rHandsontableOutput('infomat'),
      rHandsontableOutput('retiremat'),
      actionButton("go", "Plot Update"),
      numericInput("profsel", "row of profile to analyze", value = 0, min = 0, max = 0)
    ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("binplot")), 
        tabPanel("Summary", verbatimTextOutput("summary")), 
        tabPanel("Table", tableOutput("table")),
        tabPanel("text3", verbatimTextOutput("text3"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  indat_info <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(rhandsontable(empty_dat))
    raw_input = read.csv(inFile$datapath, header=T, sep = ",")
    return(rhandsontable(raw_input))
  })
  
  output$infomat <- renderRHandsontable({
    indat_info()
  })
  
  livedata_info <- eventReactive(input$go, {
    live_data_info = hot_to_r(input$infomat)
    return(live_data_info)
  })
  
  indat_retire <- reactive({
    inFile <- input$file2
    if (is.null(inFile))
      return(rhandsontable(empty_dat))
    raw_input = read.csv(inFile$datapath, header=T, sep = ",")
    return(rhandsontable(raw_input))
  })
  
  output$retiremat <- renderRHandsontable({
    indat_retire()
  })
  
  livedata_retire <- eventReactive(input$go, {
    live_data_retire = hot_to_r(input$retiremat)
    return(live_data_retire)
  })
  
  
  dumpmat <- reactive({
    for (profile in livedata_info()$Profile){
      
      dump[[profile]] = NA
      nfte <- as.numeric(livedata_info()[livedata_info()$Profile==profile,'FTE'])
      MVT <- as.numeric(livedata_info()[livedata_info()$Profile==profile,'MVT'])
      attrP <- as.numeric(livedata_info()[livedata_info()$Profile==profile,'Pattr'])
      ub <- 0.95
      lb <- 0.05
      
      df <- data.frame('Y0' = rep(nfte,nruns))
      df2 <- data.frame(row.names = c('ub','lb','probcross','naive'))
      df2$T0 <- c(nfte,nfte,0,nfte)
      
      for (y in 1:timescale){
        col <- paste0('Y',y)
        df[[col]] = NA
        df[df[,y]-livedata_retire()[livedata_retire()$Profile==profile,col] < 1,y+1] = 0
        df[df[,y]-livedata_retire()[livedata_retire()$Profile==profile,col] >= 1,y+1] = df[df[,y]-livedata_retire()[livedata_retire()$Profile==profile,col] >= 1,y] - livedata_retire()[livedata_retire()$Profile==profile,col] - rbinom(length(df[df[,y]-livedata_retire()[livedata_retire()$Profile==profile,col] >= 1,y]),df[df[,y]-livedata_retire()[livedata_retire()$Profile==profile,col] >= 1,y] - livedata_retire()[livedata_retire()$Profile==profile,col],attrP)
        colval <- c(quantile(df[[col]],c(ub,lb)),
                    sum(df[[col]] < MVT)/nruns,
                    nfte*((1-attrP)^y))
        df2[[col]] = colval
      }
      
      
      tdf2 <- data.frame(t(df2))
      dump[[profile]] = tdf2
      
      dumpmat <- dump
    }
  })
  
  
  
  observe({
    updateNumericInput(inputId = "profsel", min = 1, max = nrow(livedata_info()), value = 1)
  })
  
  selectedrow <- reactive(livedata_info()[input$profsel,])
  
  output$table <- renderTable(dumpmat()[[selectedrow()$Profile]])
  
  output$text3 <- renderPrint(dumpmat())
  
  
}

shinyApp(ui, server)