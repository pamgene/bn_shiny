 

shinyServerRun <- function(input, output, session, context) {
  
  output$body = renderUI({
    mainPanel(
      h4("testShinyShowResult"),
      verbatimTextOutput("folderOutput"),
      verbatimTextOutput("propertiesAsMapOutput"),
      verbatimTextOutput("propertiesOutput"),
      verbatimTextOutput("dataOutput"),
      verbatimTextOutput("computationResult"),
      actionButton("startComputation", "Start computation")
    )
  }) 
  
  getFolderReactive = context$getFolder()
  getPropertiesAsMapReactive = context$getPropertiesAsMap()
  getPropertiesReactive = context$getProperties()
  getDataReactive = context$getData()
  
  observe({
    
    getFolder = getFolderReactive$value
    if (is.null(getFolder)) return()
    folder = getFolder()
    
    output$folderOutput = renderText({
      folder
    })
    
    getPropertiesAsMap=getPropertiesAsMapReactive$value
    if (is.null(getPropertiesAsMap)) return()
    propertiesAsMap = getPropertiesAsMap()
    
    output$propertiesAsMapOutput = renderText({
      renderPrint({ propertiesAsMap })()  
    })
    
    getProperties=getPropertiesReactive$value
    if (is.null(getProperties)) return()
    properties = getProperties()
    
    output$propertiesOutput = renderText({
      renderPrint({ properties })()
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
      withProgress(message = "Computing", value = 0, {
        
        # Number of times we ll go through the loop
        n <- 10
        
        for (i in 1:n) {
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste("Doing part", i))
          
          # Pause for 0.3 seconds to simulate a long computation.
          Sys.sleep(0.3)
        }
      })
      result = plyr::ddply(pData(data), c("colSeq", "rowSeq"), plyr::summarize, AVG = mean(value))
      context$setResult(result)
      renderPrint({ result })()
    })
    
  })
}


shinyServerShowResults <- function(input, output, session, context) {
  
  output$body = renderUI({
    mainPanel(
      h4("testShinyShowResult"),
      verbatimTextOutput("folderOutput"),
      verbatimTextOutput("propertiesAsMapOutput"),
      verbatimTextOutput("propertiesOutput"),
      verbatimTextOutput("dataOutput")
    )
  }) 
  
  getFolderReactive = context$getFolder()
  getPropertiesAsMapReactive = context$getPropertiesAsMap()
  getPropertiesReactive = context$getProperties()
  getDataReactive = context$getData()
  
  observe({
    
    getFolder = getFolderReactive$value
    if (is.null(getFolder)) return()
    folder = getFolder()
    
    output$folderOutput = renderText({
      folder
    })
    
    getPropertiesAsMap=getPropertiesAsMapReactive$value
    if (is.null(getPropertiesAsMap)) return()
    propertiesAsMap = getPropertiesAsMap()
    
    output$propertiesAsMapOutput = renderText({
      renderPrint({ propertiesAsMap })()
    })
    
    getProperties=getPropertiesReactive$value
    if (is.null(getProperties)) return()
    properties = getProperties()
    
    output$propertiesOutput = renderText({       
      renderPrint({ properties })()
    })
    
    getData=getDataReactive$value
    if (is.null(getData)) return()
    data = getData()
    
    output$dataOutput = renderText({
      renderPrint({ data })()
    })
  })
}