library(bnshiny)
library(rtson)

context("BN shiny test")

test_that("Client addOperator", {
  print(getwd())
  testClient = TestClient$new("http://127.0.0.1:6042")
  text <- readLines("../../bn/operator/demo_operator.R",encoding="UTF-8")
  code = paste(text, collapse="\n")
  testClient$addOperator("demo_operator", code)
  testClient$getProperties("demo_operator")
  
  properties = list()
  folder = ""
  df = data.frame(colseq=c(1,1),rowseq=c(1,2),sample=c("s1", "s2"),values=c(1.2,1.5))
  metaData = data.frame(labelDescription=c("colseq","rowseq","sample","values"))
  annotatedDataframe = AnnotatedDataFrame(data=df, varMetadata=metaData)
  result = testClient$dataFrameOperator("demo_operator", annotatedDataframe, properties, folder)
  print(result)
})
 
