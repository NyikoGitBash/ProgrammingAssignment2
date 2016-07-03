## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## 1. makeCacheMatrix: This function creates a special "matrix" object that 
##    can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        Inverse <- NULL
        setMatrix <- function(y) {
                x <<- y
                Inverse <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(solve) Inverse <<- solve
        getInverse <- function() Inverse
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
        
}

## 2. cacheSolve: This function computes the inverse of the special "matrix" 
##    returned by makeCacheMatrix above. If the inverse has already been calculated 
##    (and the matrix has not changed), then the cachesolve should retrieve the 
##    inverse from the cache

cacheSolve <- function(x, ...) {
        Inverse <- x$getInverse()
        if(!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        data <- x$getMatrix()
        Inverse <- solve(data)
        x$setInverse(Inverse)
        Inverse
        ## Return a matrix that is the inverse of 'x'
}
