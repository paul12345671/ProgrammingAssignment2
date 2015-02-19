## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##############################################################################
# makeCacheMatrix simply stores the value of the inverse.  It has 'getter'   #
# and 'setter'methods to get and set the value of the inverse.  It also      #
# stores the value of the matrix in the variable x once this has been passed #
# to the function.                                                           #
##############################################################################

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

##############################################################################
# cacheSolve is passed the function makeCacheMatrix in its parameter x. Once #
# cacheSolve has calculated the inverse of the matrix, it can then use 'x'   #
# to store this value and retrieve it if needed in the future.               #
##############################################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
