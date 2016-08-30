## caches matrix with inverse then either calls cache (if possible) or calculates inverse

## stores the inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}


## calls the inverse from storage or calculates inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
