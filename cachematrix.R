## This pair of functions cache and compute an inverse matrix where
## the first function gets and sets the value of a matrix and it's inverse, 
## and the second function sets the inverse function from the first return.

## makeCacheMatrix takes one arguement, of the class matrix, and uses 
## lexical scoping lapply and a super-assignment operator so that
## it may be used in the second function, aka a different environment from 
## the one in which it was created.

makeCacheMatrix <- function(x = matrix()) {
  i <<- NULL
  set <- function(y){
    x <<- y
    i <<-NULL
  }
  get <- function() x
  getInverse <<- lapply(x, function(solve) solve(x)) 
  list(get=get, getInverse=getInverse[1])
}

## cacheSolve retrieves an inverse from cache, if it exists, using a 
## variable defined in the first function. If not,
## then it computes the inverse.

cacheSolve <- function(x, ...) {
  j <- getInverse[1]
  if(!is.null(j)){
    message("Getting cached data, hold please.")
    return(j)
  }
  data <- x$get()
  j <- solve(data, ...)
  x$getInverse(j)
  j
}
