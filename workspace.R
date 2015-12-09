   

bnshiny::startBNShiny()
 
######################################################################################
my.pkgs <- c("shiny")
pkgs <- pkgDep(my.pkgs, suggests = TRUE, enhances=FALSE)

dg <- makeDepGraph(my.pkgs, includeBasePkgs=FALSE, suggests=TRUE, enhances=TRUE)
set.seed(1)
plot(dg, legendPosEdge = c(-1, 1), legendPosVertex = c(1, 1), vertex.size=20)
######################################################################################

testClient = TestClient$new("http://127.0.0.1:6042")
text <- readLines("./bn/operator/demo_operator.R",encoding="UTF-8")
code = paste(text, collapse="\n")
testClient$addOperator("demo_operator", code)
testClient$getProperties("demo_operator")

# http://127.0.0.1:6042/?operatorId=./bn/operator/demo_operator.R&sessionType=run&reload=true
# http://127.0.0.1:6042/?operatorId=./bn/operator/demo_operator.R&sessionType=show&reload=true
# http://127.0.0.1:6042/operator/properties/?operatorId=./bn/operator/demo_operator.R&reload=true
# http://127.0.0.1:6042/operator/showResults/?operatorId=./bn/operator/demo_operator.R&reload=true

library(bnshiny)

testClient = TestClient$new("http://127.0.0.1:6042")
testClient$getProperties("./bn/operator/demo_operator.R")

properties = list()
folder = ""
df <- data.frame(x=1:6,
                 y=rep(c("Low", "High"),3),
                 z=I(LETTERS[1:6]),
                 row.names=paste("Sample", 1:6, sep="_"))
metaData <-
  data.frame(labelDescription=c(
    "Numbers",
    "Factor levels",
    "Characters"))
annotatedDataframe = AnnotatedDataFrame(data=df, varMetadata=metaData)


tson = annotated.data.frame.asTSON(annotatedDataframe)
tson$data
data.frame.fromTSON(tson$data)
annotated.data.frame.fromTSON(tson)

testClient$dataFrameOperator("./bn/operator/demo_operator.R", annotatedDataframe, properties, folder)

bin1 = writeBin(c(1.0,2.3), raw(0), size=4, endian =  "little")
bin2 = writeBin(c(1.0,2.3), raw(0), size=4, endian =  "little")
c(bin1,bin2)
 

shinyNSEnv = asNamespace( "shiny" ) 
shinyHandlerManager = get( "handlerManager" , envir = shinyNSEnv ) 
routeHandler = get( "routeHandler" , envir = shinyNSEnv ) 

# routeHandler(path, appHandlers$http)

httpHandler = function(req){
  request = HttpRequest$new(req)
  print(request$method())
  print(request$pathInfo())
  print(request$queryString())
  print(request$queryParameters())
  #   print(ls(req))
  #   print(req$PATH_INFO)
  #   print(req$HTTP_CONTENT_TYPE)
  #   print(parseQueryString(req$QUERY_STRING))
  return(list(status = 200L,
              headers = list(
                'Content-Type' = 'text/plain'
              ),
              body = 'hello'))
}
shinyHandlerManager$addHandler(routeHandler("/myroute",httpHandler) , "test")
