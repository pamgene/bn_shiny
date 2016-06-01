
shinyServerShowResults <- function(input, output, session, context) {
  
  if(!require(reshape)){
    install.packages("reshape")
  }
  
  output$body = renderUI({
    mainPanel(
      h4("Set crosstab order"),
      verbatimTextOutput("folderOutput"),
      verbatimTextOutput("dataOutput"),
      verbatimTextOutput("computationResult"),
      actionButton("startComputation", "Set crosstab order")
    )
  }) 
  
  getFolderReactive = context$getFolder()
  getDataReactive = context$getData()
  
  observe({
    
    getFolder = getFolderReactive$value
    if (is.null(getFolder)) return()
    folder = getFolder()
    
    output$folderOutput = renderText({
      folder
    })
    
    getData=getDataReactive$value
    if (is.null(getData)) return()
    data = getData()
    
    output$dataOutput = renderText({
      renderPrint({ data })()
    })
    
    computationResult = eventReactive(input$startComputation, {1})
    
    output$computationResult = renderText({
      if (computationResult() != 1 ) return()
      
      aMatrix = as.matrix(reshape::cast(pData(data), rowSeq ~ colSeq , mean))
      aHCluster = hclust(dist(aMatrix) , method="complete")
      aDend = as.dendrogram(aHCluster)
      aHCluster2 = hclust(dist(t(aMatrix))  , method="complete")
      aDend2 = as.dendrogram(aHCluster2)
      
      rowOrder = rev(order.dendrogram(aDend))
      colOrder = order.dendrogram(aDend2)
        
      context$setOrders(rowOrder, colOrder)
      renderPrint({ list(rowOrder=rowOrder, colOrder=colOrder) })()
    })
    
  })
}