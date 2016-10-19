createOperatorFromPackage = function(packageName){
  packageEnv = as.environment( paste0('package:', packageName))
  return(PackageOperator$new(packageName, env=packageEnv))
}

PackageOperator  = R6Class(
  "PackageOperator",
  inherit = Operator)

PamApp = R6Class(
  "PamApp",
  inherit = PackageOperator,
  public =list(
    
    pamAppDefinition = NULL,
    addAppRequest = NULL,
    
    initialize = function(id, pamAppDefinition){
      super$initialize(id)
      self$pamAppDefinition = pamAppDefinition
    },
    
    initializeWithSession = function(bnSession, request){
      if (!self$isInstalled()){
        self$addAppRequest = request
        self$registerInstallEnpoint(bnSession)
        req = BNOpenUrlRequest$new(self$getInstallUrl(bnSession), dialog=TRUE)
        bnSession$sendRequest(req$json)
      } else {
        bnSession$sendVoid(request$id)
      }
    },
    
    isInstalled = function() {
      pkg = self$pamAppDefinition$package
      if (!requireNamespace(pkg, quietly = TRUE)) {
        return(FALSE)
      }
      return(packageVersion(pkg) >= package_version(self$pamAppDefinition$version))
    },
    
    ensureEnvLoaded = function(){
      if (is.null(self$env)){
        self$env = loadNamespace(self$pamAppDefinition$package)
      }
    },
    
    sourceCode = function(lines){
      stop('should not implement')
    },
    
    capability = function(){
      self$ensureEnvLoaded()
      return(super$capability())
    },
    
    curveFittingTable = function(param){
      self$ensureEnvLoaded()
      return(super$curveFittingTable(param))
    },
    
    dataFrameOperator = function(operatorParam){},
    
    operatorProperties = function(){
      self$ensureEnvLoaded()
      return(super$operatorProperties())
    },
    
    showResults = function(showResultParam){
      stop('should not implement')
    },
    
    shinyServerRun = function(input, output, session, context){
      self$ensureEnvLoaded()
      return(super$shinyServerRun(input, output, session, context))
    },
    
    shinyServerShowResults = function(input, output, session, context){
      self$ensureEnvLoaded()
      return(super$shinyServerShowResults(input, output, session, context))
    },
    
    runApp = function(context){
      stop('should not implement')
    },
    
    
    registerInstallEnpoint = function(bnSession){
      bnSession$registerEndPoint(self$getInstallEndPointId(), self$shinyInstallApp)
    },
    
    unregisterInstallEnpoint = function(bnSession){
      bnSession$removeEndPoint(self$getInstallEndPointId())
    },
    
    getInstallEndPointId = function(){
      return (paste0('shinyInstallApp_',self$id))
    },
    
    getInstallUrl = function(bnSession){
      return (paste0('?sessionId=',bnSession$sessionId,'&endPointId=',self$getInstallEndPointId()))
    },
    
    shinyInstallApp = function(input, output, session, bnSession){
      
      values = reactiveValues()
      values$setupDone = 0
      values$installing = 0
      
      output$body = renderUI({
        mainPanel(
          h4("Installing package, please wait ..."),
          p(paste0("package : " , self$pamAppDefinition$package)),
          p(paste0("version : " , self$pamAppDefinition$version)),
          p(paste0("repository : " , self$pamAppDefinition$repository)),
          verbatimTextOutput("done")
        )
      })
      
      observe({
        if (isolate(values$installing) == 0){
          invalidateLater(100, session)
          values$installing = 1
          return()
        }
      })
      
      output$done = renderText({
        if (values$installing == 0){
          return()
        }
        
        
        tryCatch({
          
          self$install()
          if (!is.null(self$addAppRequest)){
            
            req = BNCloseUrlRequest$new(self$getInstallUrl(bnSession))
            bnSession$sendRequest(req$json)
            self$unregisterInstallEnpoint(bnSession)
            
            # return call of initializeWithSession
            bnSession$sendVoid(self$addAppRequest$id)
            self$addAppRequest = NULL
            
          }
        }, error = function(e) {
          traceback(e)
          if (!is.null(self$addAppRequest)){
            bnSession$sendError(self$addAppRequest$id, e)
          } else {
            bnSession$sendNoContextError(e)
          }
          
          self$unregisterInstallEnpoint(bnSession)
          bnSession$removeOperator(self$id)
          stop(e)
        } )
        renderPrint({ 'done' })()
      })
    },
    
    install = function(){
      tryCatch({
        pkg = self$pamAppDefinition$package
        if (isNamespaceLoaded(pkg)){
          currentVersion = currentPackageVersion(pkg)
          detach_package(pkg, character.only = TRUE)
          if (isNamespaceLoaded(pkg)){
            stop(paste0('failed to unload package ', pkg, ' version ', currentVersion))
          }
        }
        # go and grab latest version
        install.packages(self$pamAppDefinition$package)
        # check if correct version is installed
        if (!isInstalled()){
          msg = paste('required version ',
                      self$pamAppDefinition$version, 
                      'installed version',
                      packageVersion(self$pamAppDefinition$package))
          stop(msg)
        }
      } , error = function(e) {
        msg = paste('Failed to install package',
                    self$pamAppDefinition$package,
                    'version',
                    self$pamAppDefinition$version ,
                    ':',
                    toString(e))
        stop(msg)
      })
      
    } 
  )
)