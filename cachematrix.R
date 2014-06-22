## These functions combined allow a matrix to be created and stored in cache. It allows faster calculations as if the matrix
## is not changed the answers for previous calculations can be retrieved from the cache avoiding re-calculations.

## This function creates matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the matrix from the makeCacheMatrix function above, if the matrix does not change the function retrieves the cached matrix from the previous calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m}
