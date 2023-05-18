## Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(newValue) {
    x <<- newValue
    inverse <<- NULL  # Invalidate the cache
  }
  
  get <- function() {
    x
  }
  
  cacheInverse <- function() {
      if (!is.null(inverse)) {
      message("Getting cached inverse")
      return(inverse)
    }
    
    inverse <- solve(x)
    
    message("Caching inverse")
    setinverse(inverse)
    
   inverse
  }
  
  list(set = set, get = get, cacheInverse = cacheInverse)
}

## Compute the inverse of the special matrix object, using caching
cacheSolve <- function(x, ...) {
   if (!is.null(x$getinverse())) {
    message("Getting cached inverse")
    return(x$getinverse())
  }
  
   inverse <- x$cacheInverse()
  
    inverse
}

