## the first function creates a special "matrix" object that can cache its inverse
## the second function computes the inverse of the special "matrix" returned by
## the first function.  If the inverse has already been calculated then it will
## retreive the cached inverse.

## The first function that creates a matrix object
## makeCacheMatrix() creates the getters and setters for the matrix object so
## the cacheSolve() function can access those values inside the matrix object


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
    x <<- y
    inv <<- NULL
  }
    get <- function() x
    
## solves the inverse
    setInverse <- function() inv <<- solve(x)
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Determines if a matrix object has already had it's inverse calculated, and if
## not it calculates them caches the inverse matrix.

cacheSolve <- function(x, ...){
      inv <- x$getInverse()
      if(!is.null(inv)) {
         message("Getting cached inverse")
          return(inv)
      }
    
      answer <- x$get()
      inv <- solve(answer, ...)
      x$setInverse(inv)
      inv
}





