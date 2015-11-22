
## functions for calculating Matrix Inverse in a optimized manner

##  This function creates a special "matrix" object that can cache its inverses. 
##  

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function would get the response from Cache if available else it would calculate the 
## inverse and display the value
## cacheSolve Accepts a invertible Matrix 'x'
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  x$setinv(m)
  m
}
