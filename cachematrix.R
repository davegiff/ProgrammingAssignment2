## designed to create a special object that 
## stores a matrix and caches its inverse.

## makeCacheMatrix sets matrix, gets matrix, 
## calculates inverse and gets inverse 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  
}


## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
  }

