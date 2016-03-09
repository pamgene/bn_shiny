# MeanAndCv
# source("C:\\Users\\rdwijn.PAMGENE\\Documents\\140-700 Bioinformatics\\pamapps\\components\\MeanAndCv\\impl\\MeanAndCv.r")
library(Biobase)
checkpack <- function(packagename){
  if (! (packagename %in% installed.packages() ) ){
    install.packages(packagename, repos = "http://cran.rstudio.com/", dependencies = TRUE)
  }
  library(packagename,character.only = TRUE)
}
checkpack("ggplot2")
checkpack("plyr")
#
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
#
dataFrameOperator <- function(data,properties=properties,folder=folder) {
  metaData <- varMetadata(data)
  dataColor =  colnames(pData(data))[metaData$groupingType=="Color"]
  aD <- data.frame(rowSeq=pData(data)$rowSeq,colSeq=pData(data)$colSeq, y=data[["value"]])
  
  if (length(dataColor) > 0){
    aColorFactor = apply(pData(data) [dataColor], MARGIN = 1, FUN = paste, collapse = "-")
  } else {
    aColorFactor = "TR"
  }
  aD = data.frame(aD, c = aColorFactor)
  
  aD = aD[!data[['IsOutlier']],] # remove filtered
  
  pHigh = getPropertyValue(properties,"EM fit, high signal presence", TRUE )
  pLow = getPropertyValue(properties, "EM fit, low signal presence", TRUE)
  maxIter = getPropertyValue(properties, "EM fit, maxIterations", TRUE)
  bFit = getPropertyValue(properties, "Fit Error Model") == "Yes"
  aResult = ddply(aD, ~c, .fun = operatorFunction, pLow, pHigh, maxIter, bFit)
  
  aRunDataFile <- paste(folder, "\\runData.rda", sep = "")
  aRunDataList = list(result = aResult)
  save(file = aRunDataFile, aRunDataList)
  aFinalResult = aResult[c('rowSeq', 'colSeq', 'm', 'stdev', 'cv', 'presence', 'nominalCv')] 
  if (length(levels(aD$c)) == 1){
    aMetaData <- data.frame(labelDescription=colnames(aFinalResult), 
                            groupingType=c("rowSeq","colSeq" , "QuantitationType","QuantitationType","QuantitationType","QuantitationType","Spot") )
    anAnnotatedDataFrame <- new("AnnotatedDataFrame", data=aFinalResult, varMetadata=aMetaData) 
    return (anAnnotatedDataFrame)
  } else {
    # cannot return spot annotation
    return(aFinalResult)
  }
}#

operatorFunction<-function(aFrame, pLow = 0.05, pHigh = 0.98, maxIter = 25, do.fit = TRUE){
  # calculate per cell stats
  aResult = ddply(aFrame,  ~ rowSeq + colSeq, .fun = meanAndCvFun)
  # model parameters
  if(do.fit){
    bLow 	= aResult$m <= quantile(aResult$m, pLow);
    bHigh 	= aResult$m >= quantile(aResult$m, pHigh);
    bModel = bLow|bHigh
    doIterate = 0
    while(TRUE){
      ssq0 = pooledVarEst(aResult$varm[bLow], aResult$n[bLow]);
      lssq1 = median(aResult$lvarm[bHigh], na.rm = TRUE)
      #lssq1 = pooledVarEst(aResult$lvarm[bHigh], aResult$n[bHigh]);
      ssq1 = exp(lssq1) - 1			
      #calculate presence values for all spots
      m0 = aResult$m;
      m0[m0<0] = 0
      pres = ( sqrt(ssq1) * m0) /(sqrt(ssq1)*m0 + sqrt(ssq0))
      
      bLow = pres < pLow
      bHigh = pres > pHigh
      
      if (all(!bLow) | all(!bHigh)){
        ssq0 = NaN
        ssq1 = NaN
        break
      }
      if ( all( (bLow|bHigh) == bModel) ){
        break
      } else {
        bModel = bLow|bHigh
        doIterate = doIterate + 1
      }
      if (doIterate > maxIter){
        break
      }
    }
  }else
  {
    pres = NaN
    ssq0 = NaN
    ssq1 = NaN
  }
  
  aNewResult = data.frame(rowSeq = aResult$rowSeq,
                          colSeq = aResult$colSeq,
                          m = aResult$m,
                          stdev = aResult$stdev,
                          cv = aResult$cv,
                          presence = pres,
                          ssq0 = ssq0,
                          ssq1 = ssq1,
                          color = aFrame$c[1])
  
  aNewResult = ddply(aNewResult, ~rowSeq, .fun = nominalCv)
  return(aNewResult)
}
#
nominalCv <- function(aFrame){
  s0 = sqrt(aFrame$ssq0[1])
  s1 = sqrt(aFrame$ssq1[1])
  om = max(s0, mean(aFrame$m, na.rm = TRUE))
  ncv = cvModel(om, s0, s1)
  ncv = round(ncv, 3)
  return(data.frame(aFrame, nominalCv = ncv))
}
#
pooledVarEst<-function(s2, n){
  est = sum(s2 * (n-1), na.rm = TRUE ) / (sum(n) - length(n))
  return(est)
}
#        
meanAndCvFun <- function(aFrame){
  m = mean(aFrame$y, na.rm = TRUE)
  nReps = length(aFrame$y)
  varm = var(aFrame$y, na.rm = TRUE)
  std = sqrt(varm)
  lvarm = var(log(aFrame$y), na.rm = TRUE)
  lvarm[!is.finite(lvarm)] = NaN
  cv = std/m 
  aResult = data.frame(rowSeq = aFrame$rowSeq[1], colSeq = aFrame$colSeq[1], m = m, nReps= nReps,  stdev = std, cv = cv, varm = varm, lvarm = lvarm )   
  return(aResult)
}

cvModel <- function(x, s0, s1){
  cv = sqrt( ( (s1^2)*(x^2) + (s0^2))/(x^2) )
  return(cv)
}

cvFit = function(aFrame){
  s0 = sqrt(aFrame$ssq0[1])
  s1 = sqrt(aFrame$ssq1[1])
  label = paste(aFrame$color[1], "low signal std:", round(s0,2), "high signal cv:", round(s1,2))
  print(label)
  if ( !is.nan(s0) & !is.nan(s1)){
    aFrame = data.frame(aFrame, yFit = cvModel(aFrame$m, s0, s1), label = label)
  }
  else{
    aFrame = data.frame(aFrame, yFit = NaN, label = aFrame$color[1])
  }
  return(aFrame)
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
  aRunDataFile = paste(folder, "\\runData.rda", sep = "");
  load(aRunDataFile);
  aResult = aRunDataList$result
  xAxisMode = getPropertyValue(properties, "X-Axis Mode")
  
  if (xAxisMode == "Auto"){
    xmin = 0
    xmax =  max(as.numeric(aResult$m), na.rm = TRUE)
  } 
  else {
    xmin = getPropertyValue(properties, "Xmin", TRUE)
    xmax = getPropertyValue(properties, "Xmax", TRUE)
  }
  yAxisMode = getPropertyValue(properties, "Y-Axis Mode")
  if (yAxisMode == "Auto"){
    ymin = 0
    ymax = 1
  } 
  else {
    ymin = getPropertyValue(properties, "Ymin", TRUE)
    ymax = getPropertyValue(properties, "Ymax", TRUE)
  }  
  
  aResult = subset(aResult, m > 0)
  pHigh = getPropertyValue(properties,"EM fit, high signal presence", TRUE )
  aResult = data.frame(aResult, bHigh = aResult$presence > pHigh)
  showFit = getPropertyValue(properties, "Fit Error Model") == "Yes"
  
  
  if (showFit){
    aResult = ddply(aResult, ~color, .fun = cvFit)
  }
  
  logx = getPropertyValue(properties, "X axis scaling") == "Log"
  if (logx) {
    aResult$m = log2(aResult$m)
  }
  
  
  if (showFit & any(!is.nan(aResult$yFit))){
    prt = ggplot(aResult, aes(x = m, y = cv, colour = presence, shape = bHigh)) + geom_point()
    prt = prt + ylim(c(ymin, ymax))
    prt = prt + geom_line(aes(x = m, y = yFit), colour = "red")
    prt = prt + facet_wrap(~label)
  } else {
    prt = ggplot(aResult, aes(x = m, y = cv) ) + geom_point(colour = "blue")
    prt = prt + ylim(c(ymin, ymax))
    prt = prt + facet_wrap(~c)
  }
  
  print(prt)	
}
#




shinyServerShowResults <- function(input, output, session, context) {
  
  getFolderReactive = context$getFolder()
  getDataReactive = context$getData()
  getPropertiesReactive = context$getProperties()
  
  observe({
    getFolder = getFolderReactive$value
    if (is.null(getFolder)) return(NULL)
    folder = getFolder()
    
    getData=getDataReactive$value
    if (is.null(getData)) return(NULL)
    data = getData()
    
    getProperties=getPropertiesReactive$value
    if (is.null(getProperties)) return(NULL)
    properties = getProperties()
    
    aRunDataFile = paste(folder, "\\runData.rda", sep = "");
    load(aRunDataFile);
    aResult = aRunDataList$result
    xAxisMode = getPropertyValue(properties, "X-Axis Mode")
    
    if (xAxisMode == "Auto"){
      xmin = 0
      xmax =  max(as.numeric(aResult$m), na.rm = TRUE)
    } 
    else {
      xmin = getPropertyValue(properties, "Xmin", TRUE)
      xmax = getPropertyValue(properties, "Xmax", TRUE)
    }
    yAxisMode = getPropertyValue(properties, "Y-Axis Mode")
    if (yAxisMode == "Auto"){
      ymin = 0
      ymax = 1
    } 
    else {
      ymin = getPropertyValue(properties, "Ymin", TRUE)
      ymax = getPropertyValue(properties, "Ymax", TRUE)
    }  
    
    aResult = subset(aResult, m > 0)
    pHigh = getPropertyValue(properties,"EM fit, high signal presence", TRUE )
    aResult = data.frame(aResult, bHigh = aResult$presence > pHigh)
    showFit = getPropertyValue(properties, "Fit Error Model") == "Yes"
    
    
    if (showFit){
      aResult = ddply(aResult, ~color, .fun = cvFit)
    }
    
    logx = getPropertyValue(properties, "X axis scaling") == "Log"
    if (logx) {
      aResult$m = log2(aResult$m)
    }
    
    
    if (showFit & any(!is.nan(aResult$yFit))){
      prt = ggplot(aResult, aes(x = m, y = cv, colour = presence, shape = bHigh)) + geom_point()
      prt = prt + ylim(c(ymin, ymax))
      prt = prt + geom_line(aes(x = m, y = yFit), colour = "red")
      prt = prt + facet_wrap(~label)
    } else {
      prt = ggplot(aResult, aes(x = m, y = cv) ) + geom_point(colour = "blue")
      prt = prt + ylim(c(ymin, ymax))
      prt = prt + facet_wrap(~c)
    }
    
    output$plot = renderPlot({print(prt)})
    
  })
  
  output$body = renderUI({
    
    mainPanel(
      HTML('<div class="container">'),
      h4("Mean and CV"),
      plotOutput("plot"),
      HTML('</div>')
    )
  }) 
  
  
}
#



