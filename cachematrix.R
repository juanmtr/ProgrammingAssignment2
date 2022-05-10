## Creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) i <<- inverse
   getinverse <- function() i
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Computes the inverse of the special "matrix". It first checks if the inverse
## has already been computed. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it computes the inverse of the matrix and sets the
## value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
   i <- x$getinverse()
   if(!is.null(i)) {
      return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   i
}
