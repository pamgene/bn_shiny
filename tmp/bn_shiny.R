# library(rtson)
# 
# ClientSession <- R6Class(
#   "ClientSession",
#   public = list(
#     ws = NULL,
#     id = NULL,
#     initialize = function(ws, id){
#       self$ws = ws
#       self$id = id
#       self$ws$onMessage(self$onMessage)
#     },
#     onMessage = function(binary, message) {
#       # msg = fromTSON(message)
#     }
#   )
# )
# 
# OperatorServerDispatcher <- R6Class(
#   "OperatorServerDispatcher",
#   inherit = ShinySessionDispatcher,
#   public = list( 
#      
#     operatorServerBuilder = NA,
#     
#     initialize = function(operatorServerBuilder) {
#        
#       if (is.null(operatorServerBuilder)){
#         self$operatorServerBuilder <- OperatorServerBuilderImpl$new()
#       } else {
#         if (!inherits(operatorServerBuilder, "OperatorServerBuilder"))
#           stop("OperatorServerDispatcher : 'operatorServerBuilder' is not a OperatorServerBuilder object.")
#         self$operatorServerBuilder <- operatorServerBuilder
#        
#         shinyHandlerManager = shiny:::handlerManager
#         routeHandler = shiny:::routeHandler
#         routeWSHandler = shiny:::routeWSHandler
#         
#          
#         shinyHandlerManager$addHandler(routeHandler("/operator/runApp",self$runAppHttpHandler) , "operator_runuApp")
#          
#         shinyHandlerManager$addHandler(routeHandler("/operator/properties",self$operatorPropertiesHttpHandler) , "operator_properties")
#         shinyHandlerManager$addHandler(routeHandler("/operator/hasOperator",self$hasOperatorHttpHandler) , "operator_hasOperator")
#         shinyHandlerManager$addHandler(routeHandler("/operator/capability",self$operatorCapabilityHttpHandler) , "operator_capability")
#         
#         shinyHandlerManager$addHandler(routeHandler("/operator/addOperator",self$addOperatorHttpHandler) , "addOperator")
#         shinyHandlerManager$addHandler(routeHandler("/operator/dataFrameOperator",self$dataFrameOperatorHttpHandler) , "operator_dataFrameOperator")
#         shinyHandlerManager$addHandler(routeHandler("/operator/curveFitOperator",self$curveFitOperatorHttpHandler) , "operator_curveFitOperator")
#         shinyHandlerManager$addHandler(routeHandler("/operator/showResults",self$showResultsHttpHandler) , "operator_showResults")
#       }
#     },
#     
#     getOperatorServerBuilder = function(query){
#       return(self$operatorServerBuilder)
#       },
#     
#     
#     
#     getSessionShinyServer = function(session){
#       query <- isolate(parseQueryString(session$clientData$url_search))
#       server = self$getOperatorServerBuilder(query)$getOperatorServer(query)
#       return (server)
#     },
#     
#     addOperatorHttpHandler = function(req){
#       
#       request = HttpRequest$new(req)
#       params = rtson::fromTSON(request$read())
#       
#       self$operatorServerBuilder$addOperator(params$operatorId, params$code)
#       return(list(status = 200L,
#                   headers = list('Content-Type' = 'application/json'),
#                   body = ''))
#     },
#     operatorPropertiesHttpHandler = function(req){
#       request = HttpRequest$new(req)
#       queryParameters = request$queryParameters()
#       server = self$operatorServerBuilder$getOperatorServer(queryParameters)
#       if (is.null(server)){
#         return(list(status = 404L,
#                     headers = list('Content-Type' = 'application/json'),
#                     body = ''))
#       } else {
#         return (server$operatorPropertiesHttpHandler(request))
#       }
#     },
#     hasOperatorHttpHandler = function(req){
#       
#       request = HttpRequest$new(req)
#       queryParameters = request$queryParameters()
#       if (self$operatorServerBuilder$hasOperatorServer(queryParameters)){
#         return(list(status = 200L,
#                     headers = list('Content-Type' = 'application/json'),
#                     body = ''))
#       } else {
#         return(list(status = 404L,
#                     headers = list('Content-Type' = 'application/json'),
#                     body = ''))
#       }
#     },
#     operatorCapabilityHttpHandler = function(req){
#       
#       request = HttpRequest$new(req)
#       queryParameters = request$queryParameters()
#       server = self$operatorServerBuilder$getOperatorServer(queryParameters)
#       if (is.null(server)){
#         return(list(status = 404L,
#                     headers = list('Content-Type' = 'application/json'),
#                     body = ''))
#       } else {
#         return (server$operatorCapabilityHttpHandler(request))
#       }
#     } ,
#     
#     dataFrameOperatorHttpHandler = function(req){
#       
#       request = HttpRequest$new(req)
#       queryParameters = request$queryParameters()
#       server = self$operatorServerBuilder$getOperatorServer(queryParameters)
#       if (is.null(server)){
#         return(list(status = 404L,
#                     headers = list('Content-Type' = 'application/json'),
#                     body = ''))
#       } else {
#         return (server$dataFrameOperatorHttpHandler(request))
#       }
#     },
#     curveFitOperatorHttpHandler = function(req){
#       request = HttpRequest$new(req)
#       queryParameters = request$queryParameters()
#       server = self$operatorServerBuilder$getOperatorServer(queryParameters)
#       if (is.null(server)){
#         return(list(status = 404L,
#                     headers = list('Content-Type' = 'application/json'),
#                     body = ''))
#       } else {
#         return (server$curveFitOperatorHttpHandler(request))
#       }
#     },
#     showResultsHttpHandler = function(req){
#       request = HttpRequest$new(req)
#       queryParameters = request$queryParameters()
#       server = self$operatorServerBuilder$getOperatorServer(queryParameters)
#       if (is.null(server)){
#         return(list(status = 404L,
#                     headers = list('Content-Type' = 'application/json'),
#                     body = ''))
#       } else {
#         return (server$showResultsHttpHandler(request))
#       }
#     },
#     runAppHttpHandler = function(req) {
#       request = HttpRequest$new(req)
#       queryParameters = request$queryParameters()
#       server = self$operatorServerBuilder$getOperatorServer(queryParameters)
#       if (is.null(server)){
#         return(list(status = 404L,
#                     headers = list('Content-Type' = 'application/json'),
#                     body = ''))
#       } else {
#         return (server$runAppHttpHandler(request))
#       }
#     }
#   )
# )
# 
# OperatorServerBuilder<- R6Class(
#   "OperatorServerBuilder",
#   public = list(
#     initialize = function() {
#       stop("Abstract class cannot instantiate.")
#     },
#     getOperatorServer = function(query){
#       stop("subclass responsability")
#     },
#     addOperator = function(operatorId, code){
#       stop("subclass responsability")
#     },
#     hasOperatorServer = function(query){
#       stop("subclass responsability")
#     }
#   )
# )
# 
# OperatorServerBuilderImpl<- R6Class(
#   "OperatorServerBuilderImpl",
#   inherit = OperatorServerBuilder,
#   public = list(
#     initialize = function(client) {
#       if (is.null(client)){
#         private$client <- ServerClient$new()
#       } else {
#         if (!inherits(client, "ServerClient"))
#           stop("OperatorServerBuilder : 'client' is not a ServerClient object.") 
#         private$client <- client
#       }
#       private$operatorServerList <- list()
#       private$operatorByIds = Map$new()
#     },
#     hasOperatorServer = function(query){
#       return (private$operatorByIds$containsKey(query[["operatorId"]]))
#     },
#     getOperatorServer = function(query){
#       operatorId = query[["operatorId"]]
#       reload = query[["reload"]]
#       if (!is.null(operatorId) && !identical(operatorId, "")){
#         operatorServer = private$getOperatorServerFromOperatorId(operatorId, reload)
#         return (operatorServer)
#       }
#       return (NULL)
#     },
#     addOperator = function(operatorId, code){
#       operator <- Operator$new()
#       operator$sourceCode(code)
#       operatorServer <- OperatorServer$new(private$client, operator)
#       private$operatorByIds$set(operatorId, operatorServer)
#     }
#   ),
#   private = list(
#     
#     client = NA,
#     operatorServerList = NA,
#     operatorByIds = NULL,
#     
#     getOperatorServerFromOperatorId = function(operatorId, reload) {
#       
#       operatorServer = NULL;
#       operatorServer = private$operatorByIds$get(operatorId)
#       if (!is.null(operatorServer)){
#         return (operatorServer)
#       } 
#       
#       list <- private$operatorServerList
#       cacheEntry <- NULL
#       if (length(list) > 0){
#         for (i in 1:length(list)){
#           entry <- list[[i]]
#           if (identical(entry$name, operatorId)){
#             cacheEntry = entry
#           }
#         }
#       }
#       
#       if (is.null(reload) && !is.null(cacheEntry)){
#         return (cacheEntry$value)
#       }
#       
#       if (is.null(reload)){
#         if (is.null(cacheEntry)){
#           operatorSourceCode = private$client$getOperatorSourceCode(operatorId)
#           operator <- Operator$new()
#           operator$sourceCode(operatorSourceCode)
#           operatorServer <- OperatorServer$new(private$client, operator)
#           entry = Property$new(operatorId, operatorServer)
#           list[[length(list)+1]] <- entry
#           private$operatorServerList <- list
#         } else {
#           operatorServer <- (cacheEntry$value)
#         }
#       } else {
#         operatorSourceCode = private$client$getOperatorSourceCode(operatorId)
#         operator <- Operator$new()
#         operator$sourceCode(operatorSourceCode)
#         operatorServer <- OperatorServer$new(private$client, operator)
#         if (is.null(cacheEntry)){
#           entry = Property$new(operatorId, operatorServer)
#           list[[length(list)+1]] <- entry
#           private$operatorServerList <- list
#         } else {
#           cacheEntry$value = operatorServer
#         }
#       }
#       
#       return (operatorServer)
#     }
#   )
# )
# 
