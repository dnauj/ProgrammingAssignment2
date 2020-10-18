makeCacheMatrix <- function(m = matrix()) {
  ## Defining the inverse and setting the matrix  
  i <- NULL
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Defining functions to get the matrix and the inverse
  get <- function() m
  setInverse <- function(inverse)  i <<- inverse
  getInverse <- function() i
  
  ## Listing
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## after performing makeCacheMatrix we can use cacheSolve to get 
## the inverse from the cache if it has been calculated before
cacheSolve <- function(x, ...) {
  
  ## m as the inverse of x
  m <- x$getInverse()
  
  ## if the inverse already exists
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## calculates the inverse
  data <- x$get()
  m <- solve(data, ...)
  
  ## set the inverse value and returns it
  x$setInverse(m)
  return(m)
}
