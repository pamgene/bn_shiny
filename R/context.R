BNSessionContext  <- R6Class(
  'BNSessionContext',
  public = list(
    workflowId = NULL,
    stepId = NULL,
    contextId = NULL,
    session = NULL,
    initialize = function(workflowId, stepId, contextId, session){
      self$workflowId = workflowId
      self$stepId = stepId
      self$contextId = contextId
      self$session = session
      if (is.null(self$workflowId)) stop("BNContext : workflowId is null")
      if (is.null(self$stepId)) stop("BNContext : stepId is null")
      if (is.null(self$contextId)) stop("BNContext : contextId is null")
      if (is.null(self$session)) stop("BNContext : session is null")
      if (!inherits(session, "BNSession"))
        stop("BNContext : 'session' is not a BNSession object.")
      
    },
    processRequest = function(reactiveRequest){
      if (!inherits(reactiveRequest, "BNReactiveRequest"))
        stop("BNSessionContext processRequest : 'reactiveRequest' is not a BNReactiveRequest object.")
      reactiveRequest$processContext(self)
      return(reactiveRequest$reactiveValues)
    },
    toTson = function() list(workflowId=tson.scalar(self$workflowId), stepId=tson.scalar(self$stepId), contextId=tson.scalar(self$contextId)),
    getProperties = function() self$processRequest(BNGetPropertiesRequest$new()), 
    getPropertiesAsMap = function() self$processRequest(BNGetPropertiesAsMapRequest$new()), 
    getFolder = function() self$processRequest(BNGetFolderRequest$new()),
    # getCurveFitParams = function()self$processRequest(BNGetCurveFitParamsRequest$new()),
    getData = function() self$processRequest(BNGetDataRequest$new()),
    error = function(error) self$session$sendContextError(self$contextId, error),
    
    setOrders = function(rowOrder,colOrder){
      request = BNSetOrderRequest$new()
      orders = list()
      if (!is.null(rowOrder)) orders[['rowOrder']] = as.integer(rowOrder)
      if (!is.null(colOrder)) orders[['colOrder']] = as.integer(colOrder)
      
      request$value = orders
      self$processRequest(request)
    },
    setResult = function(result){
      if (is.null(result)){
        stop('BNSessionContext setResult : result cannot be null')
      }
      if (class(result) == "data.frame" || class(result) == "AnnotatedDataFrame"){
        request = BNSetResultRequest$new()
        request$value = result
        self$processRequest(request)
      }  else {
        stop("BNSessionContext setResult : a data.frame or an AnnotatedDataFrame is expected")
      }
      
    }
  )
)