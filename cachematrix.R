## This function creates a special "matrix" object that can cache its inverse..

makeCacheMatrix <- function(x = matrix()) {
  
  inverse_m <- NULL
  set_cache <- function(y){
    x <<- y
    inverse_m <<- NULL
  }
  
  get_cache <- function() x
  setinverse <- function(i) inverse_m <<- i
  getinverse <- function() inverse_m
  
  list(set_cache=set_cache, get_cache=get_cache, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_m <- x$getinverse()
  if(!is.null(inverse_m)){
    message("retrieving cached data")
    return(inverse_m)
  }
  data <- x$get_cache()
  inverse_m <- solve(data)
  x$setinverse(inverse_m)
  inverse_m
}
