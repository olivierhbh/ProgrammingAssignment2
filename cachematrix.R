## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function that can cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
      mat <- NULL
      set <- function(y) {
            x <<- y
            mat <<- NULL
      }
      get <- function() x
      setInvMat <- function(invMat) mat <<- invMat
      getInvMat <- function() mat
      list(set = set, get = get,
           setInvMat = setInvMat,
           getInvMat = getInvMat)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      invMat <- x$getInvMat()
      if(!is.null(invMat)) {
            message("getting cached data")
            return(invMat)
      }
      data <- x$get()
      invMat <- solve(data, ...)
      x$setInvMat(invMat)
      invMat
      
}
