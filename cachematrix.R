## this file contains two functions that are designed to work together
## to implement a 'cachable matrix'.  A matrix may have various 
## associated quantities, such as its diagonal, its transpose, its
## inverse, etc.  These quantities can be computed once and cached
## for retrieval, which is preferrable to re-computation.
## This particular version of the code implements only the cached
## inverse... but the infrastructure is all there to support the
## caching of other matrix quantities (e.g. transpose, etc).


## the first function creates a list which, essentially,
## contains the data: the matrix and its cached entities
## (only the inverse at present), as well as getters() and
## setters() for the data

makeCacheMatrix <- function(x = matrix()) {
  I = NULL
  ## use set() to create/replace the matrix
  ## and clear the cache
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  ## use get() to get the matrix
  get <- function() x
  ## use setInverse() to cache the (pre-comuted) inverse
  setInverse <- function(inI) I <<- inI
  ## use getInverse() to get the cached inverse
  getInverse <- function() I
  ## build the list
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## this function accepts a 'cachable matrix' list created
## by the function above and retrieves the cached inverse
## if it exists.  If it does not, then the function 
## computes the inverse and caches it.  In either case, the
## inverse is returned.

cacheSolve <- function(x, ...) {
  I = x$getInverse()
  if ( is.null(I) )
  {
    message("computing and caching Inverse.")
    data = x$get()
    I = solve(data, ...)
    x$setInverse(I)
  }
  else
  {
    message("getting cached Inverse.")
  }
  I
}
