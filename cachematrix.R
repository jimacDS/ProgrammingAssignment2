## These functions (makeCacheMatrix and cacheSolve) work
## together to shorten the time required to retrieve the 
## inverse of a matrix by making a cache of the matrix if it
## has already been computed.

## makeCacheMatrix returns a list of functions that:
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inpInverse) inv <<- inpInverse
  getinverse <- function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve tests to see if the inverse of a matrix has
## already been stored. If it has, it returns the inverse
## using the getinverse function from makeCacheMatrix. If not,
## it returns the inverse using the setinverse function
## from makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse = solve(data, ...)
  x$setinverse(inverse)
  inverse
}
