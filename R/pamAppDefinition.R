#' @export
parseTags = function(str){
  list = unlist(strsplit(str, "[;]"))
  list = lapply(list, function(each){
    return(substr(each, 2, length(list)-1))
  })
  return(as.character(list))
}

#' @export
tagsToString = function(list){
  str = paste0('_',paste(list,collapse='_;_'), '_')
  return(str)
}

#' @export
PamAppDefinition = R6Class(
  "PamAppDefinition",
  public = list(
    type = NULL,
    name = NULL,
    version = NULL,
    date = NULL,
    webLink = NULL,
    author = NULL,
    description = NULL,
    mainCategory = NULL,
    tags = NULL,
    capabilities = NULL,
    
    package = NULL,
    repository = NULL,
    repositoryType = NULL,
    
    initialize = function(json=NULL){
      if (!is.null(json)){
        self$fromJson(json)
      } else {
        self$mainCategory = 'pamapp'
        self$tags = character()
      }
    },
    
    addTag = function(tag){
      self$tags = union(self$tags, tag)
    },
    
    removeTag = function(tag){
      self$tags = setdiff(self$tags, tag)
    },
    
    hasTag = function(tag){
      return(is.element(tag, self$tags))
    },
    
    fromJson = function(json){
      
      self$type = json$type
      self$name = json$name
      self$version = json$version
      self$description = json$description
      self$author = json$author
      self$date = json$date
      self$capabilities = json$capabilities
      
      self$webLink = json$webLink
      self$mainCategory = json$mainCategory
      
      if (is.null(json$tags)){
        self$tags = character()
      } else {
        self$tags = parseTags(json$tags)
      }
      
      self$package = json$package
      self$repository = json$repository
      self$repositoryType = json$repositoryType
    },
    
    toJson = function(){
      json = list()
      json$type = tson.scalar(self$type) 
      json$name = tson.scalar(self$name)
      json$version = tson.scalar(self$version)
      json$description = tson.scalar(self$description)
      json$author = tson.scalar(self$author)
      json$date = tson.scalar(self$date)
      json$capabilities = tson.scalar(self$capabilities)
      json$webLink = tson.scalar(self$webLink)
      json$mainCategory = tson.scalar(self$mainCategory)
      json$tags = tson.scalar(tagsToString(self$tags))
      json$package = tson.scalar(self$package)
      json$repository = tson.scalar(self$repository)
      json$repositoryType = tson.scalar(self$repositoryType)
      return(json)
    },
    
    fromFolder = function(packagePath = getwd()){
      
      filename = paste0(packagePath, '/pamapp.padf')
      if (!file.exists(filename) ) stop('No app def file, please execute createApp first')
      
      doc = xmlParse(filename)
      root = xmlRoot(doc)
      
      self$type = xmlGetAttr(root, 'type')
      self$name = xmlGetAttr(root, 'name')
      self$version = xmlGetAttr(root, 'version')
      self$description = xmlGetAttr(root, 'description')
      self$author = xmlGetAttr(root, 'author')
      self$date = xmlGetAttr(root, 'date')
      self$capabilities = xmlGetAttr(root, 'capabilities')
      
      self$webLink = xmlGetAttr(root, 'webLink')
      self$mainCategory = xmlGetAttr(root, 'mainCategory')
      
      if (is.null(xmlGetAttr(root, 'tags'))){
        self$tags = character()
      } else {
        self$tags = parseTags(xmlGetAttr(root, 'tags'))
      }
      
      self$package = xmlGetAttr(root, 'package')
      self$repository = xmlGetAttr(root, 'repository')
      self$repositoryType = xmlGetAttr(root, 'repositoryType')
    },
    
    fromPackage = function(packagePath = getwd(), repository=NULL, repositoryType='bitbucket'){
      
      filename = paste0(packagePath, "/DESCRIPTION")
      if (!file.exists(filename) ) stop('No DESCRIPTION file')
      
      x <- read.dcf(file = filename)
      
      self$package = x[1,'Package']
      self$name = x[1,'Title']
      
      self$version = x[1,'Version']
      self$description = x[1,'Description']
      self$author = x[1,'Author']
      self$date = x[1,'Date']
      
      if ('URL' %in% colnames(x)){
        self$webLink = x[1,'URL']
      } else {
        self$webLink = 'https://pamcloud.pamgene.com/wiki/Wiki.jsp?page=PamApp%20default%20help%20page'
      }
      
      if (is.null(repository)){
        self$repository = paste0('bnoperator/', self$package)
      } else {
        self$repository = repository
      }
      
      self$repositoryType = repositoryType
      
      packageEnv = as.environment( paste0('package:', self$package) )
      
      hasShinyServerRun = exists( "shinyServerRun" , envir = packageEnv )
      hasDataFrameOperator = exists( "dataFrameOperator" , envir = packageEnv )
      hasShinyServerShowResults = exists( "shinyServerShowResults" , envir = packageEnv )
      hasOperatorProperties = exists( "operatorProperties" , envir = packageEnv )
      hasCurveFitOperatorFunction = exists( "curveFitOperatorFunction" , envir = packageEnv )
      
      if (hasShinyServerRun || hasDataFrameOperator){
        if (hasOperatorProperties){
          self$type = 'RDataStepOperator'
        } else {
          stop('Function operatorProperties is required')
        }
      } else if (hasShinyServerShowResults) {
        self$type = 'RDataScript'
      } else {
        stop('Package does not export any bn app functions : shinyServerRun | dataFrameOperator | shinyServerShowResults')
      }
      
      cap = list()
      if (hasShinyServerRun){
        cap$shinyServerRun = 'shinyServerRun'
        
      }
      if (hasDataFrameOperator){
        cap$dataFrameOperator = 'dataFrameOperator'
      }
      if (hasShinyServerShowResults){
        cap$shinyServerShowResults = 'shinyServerShowResults'
      }
      if (hasCurveFitOperatorFunction){
        cap$curveFitOperatorFunction = 'curveFitOperatorFunction'
      }
      
      self$capabilities = paste(cap,collapse=';' )
    },
    
    toXML = function(){
      doc = newXMLNode("pamAppDef")
      
      addAttributes(doc, "type"=self$type)
      addAttributes(doc, "name"=self$name)
      addAttributes(doc, "version"=self$version)
      addAttributes(doc, "author"=self$author)
      addAttributes(doc, "description"=self$description)
      addAttributes(doc, "date"=self$date)
      
      addAttributes(doc, "capabilities"=self$capabilities)
      
      addAttributes(doc, "package"=self$package)
      addAttributes(doc, "repository"=self$repository)
      addAttributes(doc, "repositoryType"=self$repositoryType)
      
      if (!is.null(self$webLink)) addAttributes(doc, "webLink"=self$webLink)
      if (!is.null(self$mainCategory)) addAttributes(doc, "mainCategory"=self$mainCategory)
      if (length(self$tags)>0) addAttributes(doc, "ztag"=tagsToString(self$tags))
      
      return(paste0('<?xml version="1.0" encoding="UTF-8"?>',saveXML(doc)))
    }
  )
)