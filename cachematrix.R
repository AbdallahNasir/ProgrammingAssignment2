#This source is meant to provide caching service for matrices inversion

## This function creates an object (list) contains four functions to access
## a cached inverse matrix and the matrix itself, only setters and getters.
## The input for this function should be a matrix to begin with, or it can
## be set later.
makeCacheMatrix <- function(x = matrix()) {
  inverseX <- NULL
  set <- function(y) {
    x <<- y
    inverseX <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseX <<- inverse
  getinverse <- function() inverseX
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function gets the cached inverse stored in a cacheMatrix object,
## or if the inverse was not calculated, this function wil calculate
## it for the first time
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
