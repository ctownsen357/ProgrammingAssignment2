## Matrix cache excercise to further learn lexical scoping

## makeCacheMatrix takes a plain square matrix and sets it up with some functionality
## that will allow the inverse (solve) of the matrix to be cached and retrieved without 
## needing to be recomputed
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes a special cacheable matrix created by makeCacheMatrix
## and retrieves the cached inverse (solve) of the special matrix if available
## otherwise it computes and then caches the inverse matrix
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}

