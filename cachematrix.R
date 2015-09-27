## This pair of functions creates and accesses a matrix whose
## inverse is only calculated once, then cached for later reference.

## makeCacheMatrix creates a CacheMatrix, actually a list of functions
## that manipulates a matrix and its inverse.

## The CacheMatrix is initialized from a simple matrix provided as
## an optional function argument.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    ## whenever the original matrix is updated, delete the cached inverse
    x <<- y
    m <<- NULL
  }
  
  ## return the original matrix
  get <- function() x
  
  ## update the cached inverse
  setinverse <- function(inv) m <<- inv
  
  ## return the cached inverse
  getinverse <- function() m
  
  ## the matrix and its inverse are "hidden" behind these 4 functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve returns the inverse of a CacheMatrix.  The inverse is
## calculated on the first call; later calls return the cached inverse.

cacheSolve <- function(x, ...) {
  ## first, check the cached inverse
  m <- x$getinverse()
  if(!is.null(m)) {
    ## the cached inverse exists - we're done.
    message("getting cached data")
    return(m)
  }
  
  ## the cached matrix hasn't been set yet.
  data <- x$get()
  
  ## calculate the inverse ...
  m <- solve(data, ...)
  
  ## ...  and cache it.
  x$setinverse(m)
  
  ## return the inverse.
  m
}

## test case:
##      [,1] [,2]
## [1,]    4    3
## [2,]    1    1
sampleMatrix <- matrix(c(4,1,3,1), nrow = 2, ncol = 2)

# > solve(sampleMatrix)
#      [,1] [,2]
# [1,]    1   -3
# [2,]   -1    4
# >