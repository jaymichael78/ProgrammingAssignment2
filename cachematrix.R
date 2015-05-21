##
# NAME: Cache Matrix

#PURPOSE: A pair of functions that cache the inverse matrix

#REVISIONS:
  
#  Ver  	Date		  Author				Description
#-------	---------	----------- 	-----------------------------------
#  1.0		05/21/2015 J.STEVENS		1. CREATED THIS Function
####

## Create object for an invertable matrix.
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns the inverse of the cacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invFunc <- x$getInverse()
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
  invFunc
}
