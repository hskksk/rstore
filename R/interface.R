#' Save a object to persistent storage.
#'
#' @param  obj     object to save.
#' @param  name    object name on the storage.
#' @param  rev     revision name of the object. this argment can be NULL and then revision is determined automatically.
#' @param  backend backend storage.
#'
#' @return character vector of length 2(name and rev)
#'
#' @export
save.obj <- function(obj, name, rev=NULL, backend=NULL){
  if(is.null(backend)){
    backend <- get.default.backend()
  }
  backend$save.obj(obj, name, rev)
}

#' Load a object from persistent storage.
#'
#' @param  name    object name on the storage.
#' @param  rev     revision name of the object. this argment can be NULL and then the function loads the object of the latest revision
#' @param  backend backend storage.
#'
#' @return loaded object.
#'
#' @export
load.obj <- function(name, rev=NULL, backend=NULL){
  if(is.null(backend)){
    backend <- get.default.backend()
  }
  backend$load.obj(name, rev)
}

#' Completely remove the object from persistent storage including all revisions.
#'
#' @param  name    object name on the storage.
#' @param  backend backend storage.
#'
#' @return the name of forgot objects.
#'
#' @export
forget.obj <- function(name, backend=NULL){
  if(is.null(backend)){
    backend <- get.default.backend()
  }
  backend$forget.obj(name)
}

#' remove the object of specific revesion from persistent storage.
#'
#' @param  name    object name on the storage.
#' @param  rev     revision name of the object. this argment can be NULL and then the function loads the object of the latest revision
#' @param  backend backend storage.
#'
#' @return the name of removed object.
#'
#' @export
remove.obj <- function(name, rev, backend=NULL) {
  if(is.null(backend)){
    backend <- get.default.backend()
  }
  backend$remove.obj(name, rev)
}

#' list object names and revisions.
#'
#' @param  name    object name on the storage. this argment can be NULL and then the function returns all objects.
#' @param  rev     revision name of the object. this argment can be NULL and then the function returns all revisions.
#' @param  backend backend storage.
#'
#' @return the name of removed object.
#'
#' @export
list.obj <- function(name=NULL, rev=NULL, backend=NULL) {
  if(is.null(backend)){
    backend <- get.default.backend()
  }
  backend$list.obj(name, rev)
}

#' create revision name from config object.
#'
#' @param  config  config object specifies the revision.
#' @param  backend backend storage.
#'
#' @return the list of length 2 (rev, revision info)
#'
#' @export
get.rev.info <- function(config=NULL, backend=NULL) {
  if(is.null(backend)){
    backend <- get.default.backend()
  }
  backend$get.rev.info(config)
}
