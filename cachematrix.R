# makeCacheMatrix and cacheSolve are a pair of functions that aim to cache the inverse of a matrix

# makeCacheMatrix is a function that caches the inverse of a matrix object
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL

  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
       setinv <- function(inv) i <<- inv
       getinv <- function() i
       list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}


#he function cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...) %*% data
  x$setinv(i)
  i
}
