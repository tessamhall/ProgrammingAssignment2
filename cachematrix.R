## Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse matrix cache
  inverse <- NULL
  
  # Setter function to set the matrix value and invalidate the cache
  set <- function(newValue) {
    x <<- newValue
    inverse <<- NULL  # Invalidate the cache
  }
  
  # Getter function to retrieve the matrix value
  get <- function() {
    x
  }
  
  # Function to compute the inverse of the matrix
  cacheInverse <- function() {
    # Check if the inverse is already cached
    if (!is.null(inverse)) {
      message("Getting cached inverse")
      return(inverse)
    }
    
    # Compute the inverse
    inverse <- solve(x)
    
    # Cache the inverse
    message("Caching inverse")
    setinverse(inverse)
    
    # Return the inverse
    inverse
  }
  
  # Return a list of the functions
  list(set = set, get = get, cacheInverse = cacheInverse)
}

## Compute the inverse of the special matrix object, using caching
cacheSolve <- function(x, ...) {
  # Check if the inverse is already cached
  if (!is.null(x$getinverse())) {
    message("Getting cached inverse")
    return(x$getinverse())
  }
  
  # If not cached, compute the inverse and cache it
  inverse <- x$cacheInverse()
  
  # Return the inverse
  inverse
}
