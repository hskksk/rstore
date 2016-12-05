get.default.backend <- function(){
  Backend.rds()
}

Backend.rds <- setRefClass("Backend.rds",
  fields = list(dir = "character"),
  methods = list(
    initialize = function(..., dir="./data"){
      dir <<- dir
    },
    save.obj = function(obj, name, rev) {
      if(stringr::str_match(name, "-")){
        stop("digits, ascii and '_' can only be used for the object name.")
      }
      if(is.null(rev)){
        rev = format(Sys.time(), format="%Y%m%d_%H%M%S")
      }
      path = name_to_path(name, rev)
      if(file.exists(path)){
        stop(sprintf("object %s(rev = %s) already exists", name, rev))
      }
      if(!file.exists(dir)){
        dir.create(dir, showWarnings=FALSE, recursive=TRUE)
      }
      saveRDS(obj, path)
      c(name, rev)
    },
    load.obj = function(name, rev) {
      if(is.null(rev)){
        rev = find.latest.rev(name)
      }
      path = name_to_path(name, rev)
      loadRDS(obj, path)
    },
    forget.obj = function(name){
      pat = sprintf("%s-.+\\.rds", name)
      fs  = list.files(dir, pat, full.names=TRUE)
      file.remove(fs)
    },
    remove.obj = function(name, rev) {
      path = name_to_path(name, rev)
      file.remove(path)
      c(name, rev)
    },
    list.obj = function(name=NULL, rev=NULL) {
      if(is.null(name)){
        name = ".+"
      }
      if(is.null(rev)){
        rev = ".+"
      }
      pat = sprintf("%s-%s\\.rds", name, rev)
      fs  = list.files(dir, pat)
      splits = stringr::str_split(fs, "-|\\.", n=3, simplify=TRUE)
      names  = splits[,1]
      revs   = splits[,2]
      sprintf("%s(rev = %s)", names, revs)
    },
    get.rev.info = function(object){
      list(rev=substring(digest::digest(object, algo="sha256"), 1, 8), info=object)
    },
    name_to_path = function(name, rev){
      sprintf("%s/%s-%s.rds", dir, name, rev)
    },
    find.revs = function(name){
      pat = sprintf("%s-.+\\.rds", name)
      fs  = list.files(dir, pat)
      mtimes = file.info(sprintf("%s/%s", dir, fs))$mtime
      revs = stringr::str_replace_all(fs, sprintf("%s-|\\.rds", name), "")
      revs[order(mtimes)]
    },
    find.latest.rev = function(name){
      revs = find.revs(name)
      revs[length(revs)]
    }
  )
)
