library(shiny)

function(){
  
dataFrameOperator <- function(data,properties=properties,folder=folder) {
  df <- data.frame(factor1=c("val1", "val2"),id=c(1,2))
 return (df)  
}
   

operatorProperties <- function() {
  propList = list(  list("X-Axis Mode", list("Auto", "Manual")),
                    list("Xmin", 0),
                    list("Xmax", 65000),
                    list("Y-Axis Mode", list("Auto", "Manual")),
                    list("Ymin", 0),
                    list("Ymax", 1),
                    list("X axis scaling", list("Linear", "Log") ),
                    list("Fit Error Model", list("Yes", "No")),  
                    list("EM fit, high signal presence", 0.95),
                    list("EM fit, low signal presence", 0.05),
                    list("EM fit, maxIterations", 25)
  )
  return (propList)
}
#
showResults <- function(properties=properties, folder=folder) {
  print("showResults properties")
  print(properties)
  print("showResults folder")
  print(folder)
  edit(data.frame(test=c(1)))
}

shinyServerRun <- function(input, output, session, client) {
  output$body = renderUI({
     
    sidebarLayout(
      sidebarPanel(
         
      ),
      mainPanel(
        headerPanel('Iris k-means clustering run') 
      )
    )
    
  })
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
}

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

shinyServerShowResults <- function(input, output, session, context) {
  
  properties = context$getPropertiesAsMap()
  properties[["R function"]] <- NULL
  table = data.frame(names=names(properties), values=as.character(properties))
  
  output$body = renderUI({
    dataTableOutput('mytable')
  })
  
  output$mytable = renderDataTable({
    table
  })
  
}
  
e <- new.env()
e$dataFrameOperator <- dataFrameOperator
e$operatorProperties <- operatorProperties
e$showResults <- showResults
e$shinyServerRun <- shinyServerRun
e$shinyServerShowResults <- shinyServerShowResults
 
return (e)
}

