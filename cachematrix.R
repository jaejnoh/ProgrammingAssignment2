## Functions that set up and compute cached inverse value of a matrix.
## Since inverse computation is an expensive O(n^2) process, inverse should
## not be computed every time that it is requested.  This module implements
## an abstract data type (ADT) that computes the inverse only once and returns
## the computed inverse afterwards.

## This function sets up a list of methods (functions) which implement
## the cached Matrix ADT.
## set: Sets the matrix
## get: Gets the matrix
## setInverse: sets the inverse of the matrix
## getInverse: gets the inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setInverse <- function(computedInverse) inv <<- computedInverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function implements the inverse computation for the cached Matrix
## abstract data type.
## If inverse has already been computed, the already computed inverse is 
## returned.  Otherwise, the inverse is computed, stored and returned.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setInverse(inv)
  inv
}
