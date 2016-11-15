## Put comments here that give an overall description of what your
## functions do
## These functions creates a special "matrix" object that caches its inverse, saving the processing time to 
## recalculate the inverse

## Write a short comment describing this function
## This function creates the "matrix" object and stores functions to set and get the cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      if(! identical(x,y)) {
          x <<- y
          m <<- NULL
      }
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## this function either gets the cached value or recalculates the inverse value
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
