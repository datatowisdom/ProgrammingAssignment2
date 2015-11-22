## This Code has been written by Datatowisdom with help from Coursera community
## functions for calculating Matrix Inverse in a optimized manner

##  This function creates a special "matrix" object that can cache its inverses. 
## makeCacheMatrix includes Getter and Setter Methods to save and retrieve values  

makeCacheMatrix <- function(x = matrix()) {
  
  matr <- NULL
  set <- function(y) {
    x <<- y
    matr <<- NULL
  }
  get <- function() x
  setinv <- function(inv) matr <<- inv
  getinv <- function() matr
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function would get the response from Cache if available else it would calculate the 
## inverse and display the value
## cacheSolve Accepts a invertible Matrix 'x'
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  matr <- x$getinv()

## if m is not null then get the cached date  
  if(!is.null(m)) {
    message("getting cached data")
    return(matr)
  }
  
  data <- x$get()
  
## solve return the Inverse
  matr <- solve(data, ...)
  x$setinv(matr)
  matr
}
