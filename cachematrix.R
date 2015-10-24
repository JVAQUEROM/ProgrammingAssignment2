## This two functions are related to matrices and their inverse.

## makeCacheMatrix creates a special matrix out of a normal matrix
## It is actually a list with the matrix and for functions to get
## and set the value of the matrix, and get and set the value of
## the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  ## We don't want the inverse to be calculated and stored
  ## as this might take a longer calculation time
  set <- function(y) {
    x <<- y
    inverse <<- NULL
    ## The inverse must be turned to NULL every time
    ## the matrix changes.
  }
  get <- function() x
  setInverse <- function(inver) inverse <<- inver
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## CacheSolve retrieves the stored value of the inverse of the matrix
## but if it doesn't exist, it will calculate it.
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(nrow(x$get()) == ncol(x$get())){ # We only calculate inverses when the matrix is squared
    m <- x$getInverse()
    if(!is.null(m)) { ## We'll get a mesage if the matrix is retrieved from cache
      message("getting cached data")
      return(m) # we pass m to the function and exit it directly
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
  }
  else m <- NULL ## If the matrix is not squared we won't calculate the inverse as it doesn't exist
  m
}
