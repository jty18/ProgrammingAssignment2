## The first function creates a "matrix" which is really a list containing multiple functions.
## The second function calculates the inverse of the "matrix," but, if it has already been
## calculated, it returns the previously stored inverse from the cache.

## Initializes a matrix and its inverse that can be called from a seperate enviroment
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Returns the inverse of the passed matrix using functions stored in makeCacheMatrix
cacheSolve <- function(x, ...) {
  
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
