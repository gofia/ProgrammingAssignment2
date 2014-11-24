## Put comments here that give an overall description of what your
## functions do
# Matrix inversion is usually a costly computation 
#and their may be some benefit to caching the inverse of a matrix 
#rather than compute it repeatedly 
## Write a short comment describing this function
# makeCacheMatrix creates a list containing a function to
makeCacheMatrix <- function(x = matrix()) {
  # set the value of the matrix 
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the value of inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  # get the value of inverse of the matrix
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
 
}


## Write a short comment describing this function
# Computes the inverse of the matrix returned by makeCacheMatrix(), 
# unless the inverse has already been calculated, in which case
## it retrieves it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## get the inverse of the matrix
  i <- x$getinverse()
  # check if there is the matrix already in the cache
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # if not: get the inverse of the matrix from the data
  data <- x$get()
  i <- solve(data, ...)
  ## set the inverse of the matrix
  x$setinverse(i)
  i

}
