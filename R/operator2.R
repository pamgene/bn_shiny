PamApp = R6Class(
  "PamApp",
  inherit = Operator,
  public =list(
    
    pamAppDefinition = NULL,
    
    initialize = function(id, pamAppDefinition){
      super$initialize(id)
      self$pamAppDefinition = pamAppDefinition
    },
    
    initializeWithSession = function(bnSession){
      if (!dir.exists(self$getAppLib())){
        self$registerInstallEnpoint(bnSession)
        req = BNOpenUrlRequest$new(self$getInstallUrl(bnSession), dialog=TRUE)
        bnSession$sendRequest(req$json)
        self$install()
        req = BNCloseUrlRequest$new(self$getInstallUrl(bnSession))
        bnSession$sendRequest(req)
        self$unregisterInstallEnpoint(bnSession)
      }
    },
    
    ensureEnvLoaded = function(){
      if (is.null(self$env)){
        pkg = self$pamAppDefinition$package
        if (isNamespaceLoaded(pkg)){
          currentVersion = currentPackageVersion(pkg)
          if (currentVersion != self$pamAppDefinition$version){
            detach_package(pkg, character.only = TRUE)
            if (isNamespaceLoaded(pkg)){
              stop(paste0('Failed to unload package ', pkg, ' version ', currentVersion))
            }
            self$env = loadNamespace(pkg, lib.loc = self$getAppLibPath())
          }
        } else {
          self$env = loadNamespace(pkg, lib.loc = self$getAppLibPath())
        }
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
      return (paste0('?sessionId=',bnSession$sessionId,'&endpointId=',self$getInstallEndPointId()))
    },
    
    shinyInstallApp = function(input, output, session, bnSession){
      output$body = renderUI({
        mainPanel(
          h4("Installing package, please wait ..."),
          p(paste0("package : " , self$pamAppDefinition$package)),
          p(paste0("version : " , self$pamAppDefinition$version)),
          p(paste0("repository : " , self$pamAppDefinition$repository))
        )
      })
    },
    
    install = function(){
      lib = self$getAppLib()
      if (!dir.exists(lib)){
        dir.create(lib, recursive = TRUE)
      }
     
      devtools::with_libpaths(
        new = self$getAppLibPath(),
        devtools::install_bitbucket(self$pamAppDefinition$repository,
                                    ref=self$pamAppDefinition$version))
    },
    
    getAppLibPath = function(){
      return(unlist(c(self$getAppLib(), .libPaths())))
    },
    
    getAppLib = function(){
      baseDirectory = getOption("bn.app.package.lib", paste0(getwd(), '/apps'))
      lib = paste0(baseDirectory,
                   '/', self$pamAppDefinition$repository,
                   '/', self$pamAppDefinition$version)
      return(lib)
    }
  )
)