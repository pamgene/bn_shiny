# [1] "HTTP_ACCEPT"                    "HTTP_ACCEPT_ENCODING"           "HTTP_ACCEPT_LANGUAGE"           "HTTP_CACHE_CONTROL"            
# [5] "HTTP_CONNECTION"                "HTTP_HOST"                      "HTTP_UPGRADE_INSECURE_REQUESTS" "HTTP_USER_AGENT"               
# [9] "httpuv.version"                 "PATH_INFO"                      "QUERY_STRING"                   "REMOTE_ADDR"                   
# [13] "REMOTE_PORT"                    "REQUEST_METHOD"                 "rook.errors"                    "rook.input"                    
# [17] "rook.url_scheme"                "rook.version"                   "SCRIPT_NAME"                    "SERVER_NAME"                   
# [21] "SERVER_PORT" 

HttpRequest <- R6Class(
  "HttpRequest",
  public = list(
    request = NA,
    initialize = function(request) {
      self$request <- request
    },
    method = function(){
      return (self$request$REQUEST_METHOD)
    },
    pathInfo = function(){
      return (self$request$PATH_INFO)
    },
    queryString = function(){
      return (self$request$QUERY_STRING)
    },
    queryParameters = function(){
      return (parseQueryString(self$request$QUERY_STRING))
    },
    read = function(){
      return (self$request$rook.input$read())
    }
  )
)