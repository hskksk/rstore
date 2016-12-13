.onLoad = function(libname, pkgname){
  if(is.null(getOption("rstore.backend.opts"))){
    options("rstore.backend.opts"=list(name="rds", dir="data"))
  }
}
