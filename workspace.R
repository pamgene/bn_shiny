   library(devtools)
   library(shiny)

bnshiny::startBNShiny()

devtools::install_bitbucket("amaurel/pgpamcloud")
devtools::install_bitbucket("amaurel/pgcheckinput")
devtools::install_bitbucket("amaurel/pgmulticore")

 
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
 
 
reactValues = reactiveValues(myProp="hey")
o1 = observe({
  print('observe call')
  print(reactValues$myProp)
})

shiny:::flushReact()

reactValues$myProp = 'hey2'
