

Operator <- R6Class(
  "Operator",
  public = list(
    id = NULL,
    env = NULL,
    
    initialize = function(id, env=NULL){
      self$id = id
      self$env = env
    },
    
    isInstalled = function() {
      return(TRUE)
    },
    
    sourceCode = function(lines){
      self$env = new.env(parent = globalenv())
      fun <- sourceUTF8FromCode(lines, envir = self$env)
    },
    
#     setShinyServerRun = function(fun) self$env$shinyServerRun = fun
#     setShinyServerShowResults = function(fun) self$env$shinyServerShowResults = fun
#     setCurveFitOperatorFunction = function(fun) self$env$curveFitOperatorFunction = fun
#     setDataFrameOperator = function(fun) self$env$dataFrameOperator = fun
#     setOperatorProperties = function(fun) self$env$operatorProperties = fun
    
    capability = function(){
      answer = list()
      
      if (!is.null(self$env$shinyServerRun)){
        answer[["run"]] = tson.scalar("shiny")
      } else if (!is.null(self$env$dataFrameOperator)){
        answer[["run"]] = tson.scalar("default")
      }
      
      if (!is.null(self$env$shinyServerShowResults)){
        answer[["showResults"]] = tson.scalar("shiny")
      } else if (!is.null(self$env$showResults)){
        answer[["showResults"]] = tson.scalar("default")
      }
      if (!is.null(self$env$curveFitOperatorFunction)){
        answer[["hasCurveFitting"]] = tson.scalar(TRUE)
      }
      return(answer)
    },
    
    curveFittingTable = function(param){
      if (!inherits(param, "CurveFittingTableParam"))
        stop("'param' is not a CurveFittingTableParam object.")
      curveFitOperatorFunction = self$env$curveFitOperatorFunction
      xValues = param$xValues
      properties = param$properties
      data = as.data.frame(t(param$data))
      
      fit <- function(x) {
        yValues <- try( curveFitOperatorFunction(xValues , x ,  properties) , silent = TRUE)
        if(inherits(yValues, "try-error")){
          yValues <- xValues+NaN
        }  
        return (yValues)
      } 
      list = as.double(unlist(lapply(data, fit)))
      
      return (list)
    },
    
    dataFrameOperator = function(operatorParam){
      
      if (!inherits(operatorParam, "DataFrameOperatorParam"))
        stop("'operatorParam' is not a DataFrameOperatorParam object.")
      fun = self$env$dataFrameOperator
      if (is.null(fun)){
        return (NULL)
      } else {
        dataFrame <- fun(operatorParam$data,
                         properties=operatorParam$properties,
                         folder=operatorParam$folder)
        dataFrame = backwardCheckResult(dataFrame)
        return (dataFrame)
      }
    },
    
    operatorProperties = function(){
      fun = self$env$operatorProperties
      if (is.null(fun)){
        return (list())
      } else {
        propList = fun()
        
        return (propList)
      }
    },
    
    showResults = function(showResultParam){
      if (!inherits(showResultParam, "ShowResultParam"))
        stop("'showResultParam' is not a ShowResultParam object.")
      fun = self$env$showResults
      if (is.null(fun)){
        return (NULL)
      } else {
        fun(properties=showResultParam$properties, folder=showResultParam$folder)
        return (NULL)
      }
    },
    
    shinyServerRun = function(input, output, session, context){
      fun = self$env$shinyServerRun
      if (is.null(fun)){
        stop("operator.shinyServerRun.missing")
      } else {
        fun(input, output, session, context)
      }
    },
    
    shinyServerShowResults = function(input, output, session, context){
      fun = self$env$shinyServerShowResults
      if (is.null(fun)){
        stop("operator.shinyServerShowResults.missing")
      } else {
        fun(input, output, session, context)
      }
    },
    
    runApp = function(context){
      fun = self$env$runApp
      if (is.null(fun)){
        context$error("operator.runApp.missing")
      } else {
        tryCatch(fun(context), error = function(e){
          context$error(e)
          stop(e)
        })
      }
    }
  )
)

