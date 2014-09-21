## The functions in this file implement a caching matrix.
## It stores the matrix itself and provides a function to compute its inverse
## that automatically stores the result of this computation for later use.
##
## Usage:
##
## m <- matrix(1:4, nrow = 2, ncol = 2) # An example matrix
## cm <- makeCacheMatrix(m)             # Create a caching version of m
## inv <- cacheSolve(cm)                # Get the inverse of the matrix

## The following function takes a matrix as its argument and creates a caching
## version of it.
##
## Usage:
##
## makeCacheMatrix(m) - Creates & returns a caching matrix based on matrix m
## $get() - Returns the matrix itself
## $set(m) - Sets the matrix to m, resetting the cached inverse
## $getInverse() - Returns the inverse of the matrix
## $setInverse(inv) - Sets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse

  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function returns the inverse of a caching matrix.
## If a cached result for this computation exists, this result is returned.
## If no cached result is found, the function computes the inverse and returns
## this while also storing it for future use.
##
## Usage:
##
## cacheSolve(cm) - Given a caching matrix, returns its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)) {
    return(i)
  }
  
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
