

## A matrix x is created using makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
m_inv <- NULL
     set <- function(y) {
          x <<- y
          m_inv <<- NULL
     }
     get <- function() x
     setinverse <- function(m_inverse) m_inv <<- m_inverse
     getinverse <- function() m_inv
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}



## Inverse of matrix x is found using cacheSolve function

cacheSolve <- function(x, ...) {
m_inv <- x$getinverse()
     if(!is.null(m_inv)) {
          message("getting cached data")
          return(m_inv)
     }
     data <- x$get()
     m_inv <- solve(data, ...)
     x$setinverse(m_inv)
     m_inv
}
