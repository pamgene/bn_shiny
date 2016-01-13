library(ggplot2)

shinyServerShowResults <- function(input, output, session, client) {
  
  data = reactive({client$getData()})
  ggplotObject = reactive({buidPlot(data())})
  dataSummary = reactive({buildSummary(data())})
  
  isolate(base::print(ggplotObject()))
  
  output$body = renderUI({
    
    mainPanel(
      HTML('<div class="container">'),
      h4("Cube density"),
      plotOutput("plot"),
      h4("Cube"),
      tableOutput('cubeSummary'),
      h4("Array"),
      tableOutput('arraySummary'),
      h4("Spot"),
      tableOutput('spotSummary'),
      HTML('</div>')
    )
  }) 
  output$plot = renderPlot({print(ggplotObject())})
  output$cubeSummary = renderTable(dataSummary()$cube)
  output$arraySummary = renderTable(dataSummary()$array)
  output$spotSummary = renderTable(dataSummary()$spot)
  
}

buildSummaryFor = function(data, groupingType, rowOrColSeq){
  pDataFrame = Biobase::pData(data)
  metaData <- Biobase::varMetadata(data)
  labels = (1:length(colnames(pDataFrame))) [metaData$groupingType == groupingType]
  labelsDescription = metaData$labelDescription[labels]
  df = data.frame(pDataFrame[labels])
  df = df[pDataFrame[[rowOrColSeq]] == 1,]
  df = as.data.frame(df)
  names(df) = labelsDescription
  if (ncol(df) > 0){
    return(summary(df))
  } else {  
    return(data.frame(na=c()))
  }
}

buildSummary = function(data){
  pDataFrame = Biobase::pData(data)
  rcFrame = data.frame(Rows = as.factor(max(pDataFrame$rowSeq)),
                       Columns = as.factor(max(pDataFrame$colSeq)), N=as.factor(nrow(pDataFrame)))
  rownames(rcFrame) = '#'
  return (list( array= buildSummaryFor(data, 'Array', 'rowSeq'),
                spot=buildSummaryFor(data, 'Spot', 'colSeq'),
                cube=rcFrame  ))
}

buidPlot <- function(data) {
  pDataFrame = Biobase::pData(data)
  aD <- data.frame(rowSeq=pDataFrame$rowSeq,colSeq=pDataFrame$colSeq, y=data[['value']])
  metaData <- Biobase::varMetadata(data)
  
  xLimits = quantile(aD$y, c(0.01, 0.99), na.rm = TRUE)
  dataColor =  colnames(pDataFrame)[metaData$groupingType=='Color']
  
  if (length(dataColor) > 0){
    if( !(class(pDataFrame[[dataColor]]) == "numeric") )
    {
      aColors = apply(pDataFrame[dataColor], MARGIN = 1, FUN = paste, collapse = "-")
      aD = data.frame(aD, Factor = as.factor(aColors))
    } else {
      dataColor = NULL
    }
  }
  if (length(dataColor) == 0) {
    aD = data.frame(aD, Factor = "not.defined")
  }
  qType = colnames(pDataFrame)[metaData$groupingType=='QuantitationType']
  aD = data.frame(aD, QuantitationType = data[[qType]])
  m = ggplot(aD, aes(x = y, group = Factor, colour = Factor))
  
  m = m + geom_density(size = 1) + xlim(xLimits) + facet_wrap(~QuantitationType)
  return (m)
}