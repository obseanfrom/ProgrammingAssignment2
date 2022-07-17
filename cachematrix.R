## Put comments here that give an overall description of what your
## functions do

## User-defined function for caching the inverse of a matrix (x)

makeCacheMatrix <- function(x = matrix()) {

    ##define variables
    ix <- NULL
  
    ##we put a function in your function so it can function while it functions
    set <- function(y) {
      
      x <<- y
    ix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) ix <<- inverse
    getInverse <- function() ix
    list(
     set = set,
     get = get,
     setInverse = setInverse,
     getInverse = getInverse
        )
}


## Function which returns the inverse of a matrix (x) but checks for cached data so it can avoid redundant processing.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ix <- x$getInverse()
    if(!is.null(ix)) {
      print("getting cached data")
      return(ix)
    }
    data <- x$get()
    ix <- solve(data, ...)
    x$setInverse(ix)
    ix
}

