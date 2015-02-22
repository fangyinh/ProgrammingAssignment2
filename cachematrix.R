## This script contains a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse. Ouput matrix is a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the reverse
## 4. get the value of the reverse

makeCacheMatrix <- function(x = matrix()) {
        r <- NULL
        set <- function(y){
          x <<- y
          r <<- NULL
        }
        get <-function() x
        setreverse <- function(reverse) r <<- reverse
        getreverse <- function() r
        list(set = set, get = get,
             setreverse = setreverse,
             getreverse = getreverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        r<-x$getreverse()
        if(!is.null(r)){
          message("getting cached data")
          return(r)
        }
        data <-x$get()
        r<-solve(data, ...)
        x$setreverse(r)
        r
}
