## makeCacheMatrix and cacheSolve work together to check
## for an inverse of a matrix in cache, and compute it if
## the inverted matrix has not already been saved to cache

## makeCacheMatrix initializes the cache variable 'i', creates
## subfunctions set(), get(), setinverse() and getinverse().
## It also returns a list of values for these subfunctions.

makeCacheMatrix <- function(x = matrix()) {
 i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks to see if the matrix inverse is saved to
## cache. If so, a message is returned indicating the cached 
## value will be returned, and the cached value is returned. 
## If not, the inverse of the matrix is calculated and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
