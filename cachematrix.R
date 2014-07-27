## This is a pair of functions which allow the caching of the inverse of a square
## square invertible matrix.  The major code architecture was taken from example code
## used for caching the mean of a vector.

## This function is used to store an initial square matrix into an object, accessible
## by calling "obectname$get()".  The matrix can be changed by calling
## "objectname <- makeCacheMatrix(newmatrix)".

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks the above created object for a cached inverted matrix  and
## if the inverse matrix has not been computed, does so and stores it in the above
## created object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
