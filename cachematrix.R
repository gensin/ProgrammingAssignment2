## In this function where are creating an enviroment to manage matrices and their inverses using cache storage.

## This function is a special "matrix" which have 4 functions inside it. You can get or set the value of the inside matrix
## or set and get the inverse of the inside function. You can add whichever value you want to inverse but is better use
## the function cacheSolve to make the inverse value.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL    # i is the cache variable with the inverse of the matrix
  set <- function(y){ # Put a new value to the special matrix and restore cache
      x <<- y
      i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function calculate the inverse of the given matrix. It uses the function solve to do that, but if the inverse is 
## already done and is in the cached data it uses that cached matrix as a return value.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)){  ## If i is not null there is a cached data already calculated.
    message("Getting inverse from cache")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
