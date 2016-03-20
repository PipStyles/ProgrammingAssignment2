#' makeCacheMatrix
#' 
#' Accepts a matrix as param (should be square, but is NOT checked!)
#' Provides a collection of functions to allow caching (getting/setting) of
#' the original matrix and its inverse.
#' 
#' @param mat (square matrix i.e. equal number of rows/columns)
#' @return list of functions for getting/setting (see comments by each)
makeCacheMatrix <- function(mat = matrix()) {
  
  inv <- NULL
  
  ## set the internal matrix variable and NULL the inverse 
  ## (as it would need to be recalculated)
  setMatrix <- function(m) {
    mat <<- m
    inv <<- NULL
  }
  
  ## return the original matrix
  getMatrix <- function() {
    mat
  }
  
  
  ##pipstyles: poor design decision IMO - this function could just be a generic cache
  
  
  ## setinverse - sets the inverse
  setInverse <- function(i) {
    inv <<- i
  }
  
  ##returns the cached inverse matrix (or NULL if not yet set)
  getInverse <- function() {
    inv
  }
  
  ## return the list of nested functions
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}


#' cacheSolve
#' 
#' Checks if "special" matrix (x) cache function's inverse is NULL
#' solves if required and sets the inverse in the cache.
#'
#' @param x (list of named functions returned by makeCacheMatrix)
#' @param ... (additional params passed to solve)
#' 
#' @return inverse of matrix (retreived from cache)
#' 
cacheSolve <- function(x, ...) {
  ## Check for null...
  if(is.null(x$getInverse())) {
    message("setting cached matrix")
    x$setInverse(solve(x$getMatrix(), ...))
  }
  x$getInverse()
}
