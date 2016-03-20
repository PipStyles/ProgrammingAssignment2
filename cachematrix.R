## Pair of functions to a) cache a matrix and its inverse
##  and b) calculate and set the inverse of a matrix in the cache function

## pipstyles: I don't really like this configuration of functions. 
## makeCacheMatrix is 'confused' in naming/purpose

makeCacheMatrix <- function(mat = matrix()) {
  
  inv <- NULL
  
  setMatrix <- function(m) {
    mat <<- m
    inv <<- NULL
  }
  
  getMatrix <- function() {
    mat
  }
  
  ##poor design decisions IMO - this function could just be a generic cache
  ## and not concern itself with specific methods? Or it could just perform
  ## the solving and caching itself (but that would break single responsibility
  ## so, perhaps not)
  
  setInverse <- function(i) {
    inv <<- i
  }
  
  getInverse <- function() {
    inv
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}


## Checks if "special" matrix (x) cache function's inverse is NULL, solves if required and 
## sets in the cache.

cacheSolve <- function(x, ...) {
  ## Check for null...
  if(is.null(x$getInverse())) {
    message("setting cached matrix")
    x$setInverse(solve(x$getMatrix(), ...))
  }
  x$getInverse()
}
