
#' rds backend class
#'
#' @field dir storage directory
#'
#' @export
Backend.rds <- setRefClass("Backend.rds",
  fields = list(dir = "character"),
  methods = list(
    initialize = function(..., dir="./data"){
      dir <<- dir
    },
    save.obj = function(obj, name, rev) {
      if(stringr::str_detect(name, "[^a-zA-Z0-9_]")){
        stop("digits, ascii and '_' can only be used for the object name.")
      }
      ts = get.timestamp()
      if(is.null(rev)){
        rev = ts
      }
      if(obj.exists(name, rev)){
        stop(sprintf("object %s(rev = %s) already exists", name, rev))
      }
      if(!file.exists(dir)){
        dir.create(dir, showWarnings=FALSE, recursive=TRUE)
      }
      path = name_to_path(name, rev, ts)
      saveRDS(obj, path)
      c(name, rev)
    },
    load.obj = function(name, rev) {
      if(is.null(rev)){
        rev = get.latest.rev(name)
      }
      ts = get.obj.timestamp(name, rev)
      path = name_to_path(name, rev, ts)
      readRDS(path)
    },
    obj.exists = function(name, rev) {
      if(is.null(rev)){
        rev = get.latest.rev(name)
        if(length(rev)==0){
          return(FALSE)
        }
      }
      pat = name_to_fname(name, rev, ".+")
      fs = list.files(dir, pat)
      length(fs)>0
    },
    forget.obj = function(name){
      pat = name_to_fname(name, ".+", ".+")
      fs  = list.files(dir, pat)
      splits = path_to_name(fs)
      fs.full = sprintf("%s/%s", dir, fs)
      splits$rev[file.remove(fs.full)]
    },
    remove.obj = function(name, rev) {
      ts = get.obj.timestamp(name, rev)
      path = name_to_path(name, rev, ts)
      file.remove(path)
      c(name, rev)
    },
    forget.rev = function(rev){
      pat = name_to_fname(".+", rev, ".+")
      fs  = list.files(dir, pat)
      splits = path_to_name(fs)
      fs.full = sprintf("%s/%s", dir, fs)
      splits$name[file.remove(fs.full)]
    },
    list.obj = function(name=NULL, rev=NULL) {
      if(is.null(name)){
        name = ".+"
      }
      if(is.null(rev)){
        rev = ".+"
      }
      pat = name_to_fname(name, rev, ".+")
      fs  = list.files(dir, pat)
      splits = path_to_name(fs)
      sprintf("%s(rev = %s)", splits$name, splits$rev)[order(splits$ts)]
    },
    get.rev.info = function(object){
      list(rev=substring(digest::digest(object, algo="sha256"), 1, 8), info=object)
    },
    save.rev.info = function(object, rev){
      save.obj(object, "__REVINFO__", rev)
    },
    load.rev.info = function(rev){
      load.obj("__REVINFO__", rev)
    },
    name_to_fname = function(name, rev, ts){
      sprintf("%s-%s-%s.rds", name, rev, ts)
    },
    name_to_path = function(name, rev, ts){
      sprintf("%s/%s", dir, name_to_fname(name, rev, ts))
    },
    path_to_name = function(path){
      splits = stringr::str_split(path, "-|\\.r", n=4, simplify=TRUE)
      list(
        name = splits[,1]
        ,rev = splits[,2]
        ,ts  = splits[,3]
      )
    },
    find.revs = function(name){
      pat = name_to_fname(name, ".+", ".+")
      fs  = list.files(dir, pat)
      splits = path_to_name(fs)
      splits$rev[order(splits$ts)]
    },
    get.latest.rev = function(name){
      revs = find.revs(name)
      revs[length(revs)]
    },
    get.timestamp = function(){
      op = options(digits.secs=3)
      on.exit(options(op))
      format(Sys.time(), format="%Y%m%d_%H%M%OS")
    },
    get.obj.timestamp = function(name, rev){
      pat = name_to_fname(name, rev, ".+")
      fs = list.files(dir, pat)
      splits = path_to_name(fs)
      splits$ts
    }
  )
)

get.default.backend = function(){
  opts = getOption("rstore.backend.opts")
  do.call(create.backend, opts)
}
