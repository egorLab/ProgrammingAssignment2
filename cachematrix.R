## Put comments here that give an overall description of what your
## functions do

## This function creates an object which can cache matrix's inverse in cache.

makeCacheMatrix <- function(x = matrix()) {
  
    cache = NULL
    set <- function(y) {
      
        x <<- y
        cache <<- NULL
        
  }
    get <- function() x
    setInv <- function(inverse)
    cache <<- inverse
    getInv <- function() cache
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
  

}


## We are computing inverse or returning value from cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  cache <- x$getInv()
  
  if (!is.null(cache)) {
    
    message("getting cached data")
    return(cache)
    
  }
  
  m <- x$get()
  cache <- solve(m, ...)
  x$setInv(cache)
  
  
  cache
}
