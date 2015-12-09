##source("C:\\Users\\rdwijn.PAMGENE\\Documents\\140-700 Bioinformatics\\pamapps\\components\\Shiny PCA\\impl\\PCA.r")

library(Biobase)
checkpack <- function(packagename){
  if (! (packagename %in% installed.packages() ) ){
    install.packages(packagename, repos = "http://cran.rstudio.com/", dependencies = TRUE)
  }
  library(packagename,character.only = TRUE)
}
checkpack("rgl")
checkpack("reshape")
# checkpack("tcltk")
checkpack("shiny")
checkpack("shinyRGL")

getPropertyValue <- function(properties=properties, name=name, prop.is.numeric = FALSE) {
  for ( i in 1:length(properties) ) {
    if (properties[[i]][1] == name){
      val = properties[[i]][2]
      if (prop.is.numeric){
        return(as.numeric(val))
      } else {
        return(val)
      }
    }
  }
  stop(paste("property not found: ", name))
  return (NULL)
}

dataFrameOperator <- function(data,properties=properties,folder=folder) {
  list = internal_dataFrameOperator(data,properties=properties,folder=folder)
  return (list$result)
}

internal_dataFrameOperator <- function(data,properties=properties,folder=folder) {
  
  print("internal_dataFrameOperator")
  
  metaData <- varMetadata(data)
  grpLabels   =  colnames(pData(data))[metaData$groupingType=="Color"]
  arrayLabels   =  colnames(pData(data))[metaData$groupingType=="Array"]
  spotLabels    =  colnames(pData(data))[metaData$groupingType== "Spot"]
  aD = pData(data)
  
  if (length(arrayLabels) > 0){
    obs = droplevels(interaction(pData(data)[arrayLabels]))
  } else {
    stop("PCA requires multiple columns (observations) in he BN cross-tab view")
  }
  
  if (length(spotLabels) > 0){
    vars = droplevels(interaction(pData(data)[spotLabels]))
  } else {
    stop("PCA requires multiple rows (variables) in the BN cross-tab view")
  }
  sampleAnnotation = aD[c(arrayLabels, grpLabels)]
  sampleAnnotation = sampleAnnotation[aD$rowSeq == 1,]
  colorAnnotation = aD[c(grpLabels, arrayLabels)]
  colorAnnotation = colorAnnotation[aD$rowSeq == 1,]
  X = cast(aD,rowSeq~colSeq, value = "val")
  X = t(X)
  if (any(is.na(X))){
    stop("Missing values are not allowed")
  }
  obsNames = obs[aD$rowSeq == 1]
  varNames = vars[aD$colSeq == 1]
  
  rownames(X) = obsNames
  colnames(X) = varNames
  
  bScale = getPropertyValue(properties, "Scale Spots") == "Yes"
  aPca = prcomp(X,  scale. = bScale)
  
  rm.Comp = getPropertyValue(properties, "Subtract component", TRUE)
  if (rm.Comp > 0){
    N = scale(X, scale = bScale) - (aPca$x[,rm.Comp]) %*% (t(aPca$rotation[,rm.Comp]))
    aPca = prcomp(N, scale. = bScale)
    sVal = melt(N)
  } else {
    sVal = NULL
  }
  
  nComp = getPropertyValue(properties, "Number of Components", TRUE)
  # reformat and return the required number of loadings and scores
  aFlatRotation = matrix(nrow = dim(aD)[1],ncol = nComp)
  aFlatScore    = matrix(nrow = dim(aD)[1],ncol = nComp)
  for (j in 1:nComp) {
    for (i in 1:dim(X)[2]){
      aFlatRotation[aD$rowSeq == i,j]  = aPca$rotation[i,j]
    }
    for (i in 1:dim(X)[1]){
      aFlatScore[aD$colSeq == i,j]  = aPca$x[i,j]
    }
  }
  aResult = data.frame(aFlatRotation)  
  colnames(aResult) = paste('PC', 1:nComp, sep = '');
  aResult = data.frame(aResult, aFlatScore)
  aNames = colnames(aResult)
  aResult = data.frame(aResult, rowSeq = aD$rowSeq, colSeq = aD$colSeq)     
  aResult = aResult[order(aResult$rowSeq, aResult$colSeq),]
  if (!is.null(sVal)){
    aResult = data.frame(aResult, sVal = sVal$value)
    #     edit(aResult)
  }
  # store data for showResults
  #   aRunFile = paste(folder, '\\runData.rda', sep = '')
  
  
  aRunDataList = list(aPca = aPca, sampleAnnotation=sampleAnnotation, colorAnnotation = colorAnnotation, varNames = varNames)
  #   save(file = aRunFile, aRunDataList)
  # exit
  return(list(result=aResult, showResult=aRunDataList)) 
}

operatorProperties <- function() {
  propList = list(    list('Scale Spots', list('No', 'Yes')), 
                      list('Number of Components', 5),
                      list('Subtract component', 0)
  )    
  return (propList)
}  


shinyServerShowResults <- function(input, output, session, client) {
  
  print("shinyServerShowResults")
  
  data = reactive({print("reactive data") ; client$getData()})
  properties = reactive({print("reactive getProperties") ; client$getProperties()}) 
  folder = reactive({print("reactive getFolder") ; client$getFolder()})
   
  listResult = reactive({print("reactive listResult") ; p=properties(); internal_dataFrameOperator(data(), properties=p , folder=folder())}) 
  
  aRunDataList = reactive({print("reactive aRunDataList") ; listResult()$showResults})
  #   mapProperties = client$getPropertiesAsMap()
  
  #   aRunFile = paste(folder, '\\runData.rda', sep = '')
  #   load(aRunFile)
  aPca = reactive({print("reactive aPca") ; aRunDataList()$aPca})    
  sAnnotation = reactive({print("reactive sAnnotation") ; aRunDataList()$sampleAnnotation})  
  cAnnotation = reactive({print("reactive cAnnotation") ; aRunDataList()$colorAnnotation})
  varNames = reactive({print("reactive varNames") ; aRunDataList()$varNames})
  
  nComp = reactive({print("reactive nComp") ; getPropertyValue(properties(), "Number of Components", TRUE)})
  #   nComp = mapProperties[["Number of Components"]]
  
  px = reactive({print("reactive px") ; paste("PC", 1:nComp(), sep = "")})
  py = reactive({print("reactive py") ; px()[c(2,1,3:nComp())]})
  
  output$body = renderUI({
    
    sidebarLayout(
      #       headerPanel("Shiny PCA scores results"),
      sidebarPanel(
        selectInput("labsOrSpheres", "What to show", 
                    choices = c("Text labels", "Spheres")),
        selectInput("colorAnnotation", "Select factor for coloring", choices = colnames(cAnnotation())),
        selectInput("textAnnotation", "Select factor for text labels", choices = colnames(sAnnotation())),
        helpText(""),
        plotOutput("legend"),
        helpText(""),
        h5("Shiny PCA", a("help on PamCloud", href="http:///pamcloud.pamgene.com//wiki//Wiki.jsp?page=Shiny%20PCA") ),
        actionButton("stop", "Done")
      ),
      mainPanel(
        tabsetPanel(tabPanel("Scores 3D", webGLOutput("scores3D", height = 700)),
                    tabPanel("Scores Plot matrix",plotOutput("mat", height = 700)),
                    tabPanel("Biplot", plotOutput("biplot",height = 600, width = 600), 
                             sliderInput("showvars", "% largest variable loadings to show",min = 0, max = 100, value = 10 ),
                             sliderInput("zl","Zoom variable loadings (%)", min = 1, max = 400, value = 100), 
                             selectInput("px", "X-axis", choices = px()),
                             selectInput("py", "Y-axis", choices = py())),
                    tabPanel("Variation", plotOutput("scree"))
        )
      )
    )
    
  })
  
  cCol = reactive({
    cHdr = as.character(input$colorAnnotation)
    cGrp = as.factor(cAnnotation()[[cHdr]])
    return(1+as.numeric(cGrp))
  })
  
  sLab = reactive({
    tHdr = as.character(input$textAnnotation)
    tGrp = as.character(sAnnotation()[[tHdr]])
    return(tGrp)
  })
  
  output$mat <- renderPlot({
    clrs = cCol()
    pairs(aPca()$x[,1:nComp()], col = clrs, bg = clrs, pch = 21)
  }, height = 650, width = 850)
  
  
  output$scores3D <- renderWebGL({
    pSym = switch(input$labsOrSpheres, 
                  "Text labels" = "p",
                  "Spheres" = "s"
    )
    clrs  = cCol()
    tGrp  = sLab()
    plot3d(x = aPca()$x[,1],y = aPca()$x[,2], z = aPca()$x[,3],  col = clrs, box = FALSE, type = pSym, size = 2, xlab = "PC1", ylab = "PC2", zlab = "PC3")
    if(pSym == "p"){
      text3d(x = aPca()$x[,1], y = aPca()$x[,2], z= aPca()$x[,3], texts = tGrp, adj = 0, col = clrs, box = FALSE)
    }
  }, height = 650, width = 700)
  
  output$scree = renderPlot({
    screeplot(aPca(), main = "variance explained per component", col = "lightblue")
  })
  
  output$legend = renderPlot({
    plot(1,1, xaxt = "n", yaxt ="n", xlab = "", ylab = "", col = "white", bty = "n")
    cHdr = as.character(input$colorAnnotation)
    cGrp = as.factor(cAnnotation()[[cHdr]])
    clrs = unique(cCol())
    legend(x = "top", legend = unique(cGrp), fill = clrs, cex = 1, bty = "n") 
  })
  
  output$biplot = renderPlot({
    xIdx = c(1:nComp())[input$px == px()]
    yIdx = c(1:nComp())[input$py == px()]
    qntVars = 1-0.01*input$showvars
    zmVars = 0.01 * input$zl;
    oLabs = sLab()
    clrs = cCol()
    idx = c(xIdx, yIdx)
    obs = aPca()$x[,idx]
    vars = aPca()$rotation[,idx]
    sc = aPca()$sdev[idx]^2
    lvars = apply(vars^2, 1, sum)
    bVar = lvars > quantile(lvars, qntVars)
    vars = vars[bVar,]
    xp = c(sc[1]*vars[,1], obs[,1])
    yp = c(sc[2]*vars[,2], obs[,2])
    zm = 1.1
    scxVars = zmVars*sc[1]*vars[,1]
    scyVars = zmVars*sc[2]*vars[,2]
    plot(x = scxVars, y = scyVars, pch = ".", xlim = zm * c(min(xp), max(xp)), ylim = zm * c(min(yp), max(yp)), xlab = input$px, ylab = input$py )
    arrows(x0 = 0, y0 = 0, x = scxVars, y = scyVars, col = "grey")
    text(x = obs[,1], y = obs[,2], oLabs, col = clrs, cex = 2)
    text(x = scxVars, y = scyVars, rownames(vars), col = "black")
  })
  
  exportPlot = function(fname = "myName", ftype = ".png"){
    fileName <- tclvalue(tkgetSaveFile(filetypes = "{{png images} {*.png}}", 
                                       title = "Save 3D plot as image file", 
                                       initialfile = paste(fname,ftype, sep = "") 
    ))
    
    if (nchar(fileName)) {
      if (substr(fileName, nchar(fileName)-3, nchar(fileName)) != ftype){
        fileName = paste(fileName, ftype , sep = "")
      }
      edit(fileName)
      rgl.snapshot(filename=fileName,fmt="png")
    }
  }
  
}