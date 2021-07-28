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
source("plotfunction.R")
library(rhandsontable)

empty_dat = matrix(nrow = 1,ncol = 5)
colnames(empty_dat) = c("Profile","FTE","MVT","Pattr","RT")


empty_dat2 = matrix(nrow = 1,ncol = 10)
colnames(empty_dat2) = c("Profile",paste0('Y',rep(0:8)))

dumpthing <- list()



nruns = 10000
timescale = 8

ui <- fluidPage(
  titlePanel("Stochastic forecast of likely shortage scenarios"),
  
  sidebarLayout(
    sidebarPanel(
      
      
      fileInput("file1", "Upload infomat CSV file here", accept = c(".csv")),
      rHandsontableOutput('infomat'),
      fileInput("file2", "Upload retiremat CSV file here", accept = c('.csv')),
      rHandsontableOutput('retiremat'),
      actionButton("go", "Data Update / generate plot"),
      numericInput("profsel", "Row of profile to analyze", value = 0, min = 0, max = 0)
    ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 tableOutput("plottable"),
                 plotOutput("binplot")),
        tabPanel("whatif",
                 sliderInput('attrslider', label = "Range of attrition rates" ,min = 0.01, max = 0.3, value = c(0.05,0.1), step = 0.01),
                 actionButton("go_df", "Generate / refresh whatif table"),
                 textOutput('tabletext'),
                 tableOutput('whatiftable')),
        tabPanel("Info and readme",includeHTML("info.html"))
        
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
    raw_input = raw_input[order(raw_input$Profile),]
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
    raw_input = raw_input[order(raw_input$Profile),]
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
      
      #dumpthing[[profile]] = NA
      nfte <- as.numeric(livedata_info()[livedata_info()$Profile==profile,'FTE'])
      MVT <- as.numeric(livedata_info()[livedata_info()$Profile==profile,'MVT'])
      attrP <- as.numeric(livedata_info()[livedata_info()$Profile==profile,'Pattr'])
      ub <- 0.95
      lb <- 0.05
      
      df <- data.frame('Y0' = rep(nfte,nruns))
      df2 <- data.frame(row.names = c('ub','lb','probcross','naive'))
      df2$T0 <- c(nfte,nfte,0,nfte)
      df3 <- data.frame('Y0' = nfte)
      
      for (y in 1:timescale){
        col <- paste0('Y',y)
        df[[col]] = NA
        df3[[col]] = NA
        df[df[,y]-livedata_retire()[livedata_retire()$Profile==profile,col] < 1,y+1] = 0
        df[df[,y]-livedata_retire()[livedata_retire()$Profile==profile,col] >= 1,y+1] = df[df[,y]-livedata_retire()[livedata_retire()$Profile==profile,col] >= 1,y] - livedata_retire()[livedata_retire()$Profile==profile,col] - rbinom(length(df[df[,y]-livedata_retire()[livedata_retire()$Profile==profile,col] >= 1,y]),as.numeric(df[df[,y]-livedata_retire()[livedata_retire()$Profile==profile,col] >= 1,y] - livedata_retire()[livedata_retire()$Profile==profile,col]),attrP)
        df3[,y+1] = df3[,y]*((1-attrP))-livedata_retire()[livedata_retire()$Profile==profile,col]
        colval <- c(quantile(df[[col]],c(ub,lb)),
                    sum(df[[col]] < MVT)/nruns,
                    ifelse(df3[[col]]<0,0,df3[[col]]))
        df2[[col]] = colval
      }
      
      
      tdf2 <- data.frame(t(df2))
      dumpthing[[profile]] = tdf2
      
      
    }
    return(dumpthing)
  })
  
  
  
  observe({
    updateNumericInput(inputId = "profsel", min = 1, max = nrow(livedata_info()), value = 1)
  })
  
  selectedrow <- reactive(livedata_info()[input$profsel,])
  
  
  
  output$binplot <- renderPlot(
    binplot(df = dumpmat()[[selectedrow()$Profile]], ts = timescale, nrep = nruns, selrow = selectedrow()), width = 900, height = 600
  )
  
  plottable.t <- reactive({
    df <- t(dumpmat()[[selectedrow()$Profile]])
    rownames(df) <- c('lower bound','upper bound','probability of shortage','constant attrition')
    return(df)
  })
  
  output$plottable <- renderTable(plottable.t(),rownames=T)
  
  output$tabletext <- renderText(paste('Shortage probabilities across different attrition rates for',selectedrow()$Profile))
 
  output$whatiftable <- renderTable(whatifdf(selectedrow = selectedrow(), retiremat = livedata_retire(), attrlow = input$attrslider[1], attrhigh = input$attrslider[2]), rownames = T)
  
}

shinyApp(ui, server)