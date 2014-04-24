## Matrix inversion is computationaly very costly.
## Caching the inverse of a matrix avoids repeting this operation.
## The two functions, makeCacheMatrix() and cacheSolve(), defined below
## provide the functionality to cache the inverse of a matrix.

## makeCacheMatrix() - Creates a special matrix object that can cache 
## its inverse. 
## It defines following 4 operations on the special matrix object:
## setMatrix() - Set the matrix to the new one passed as parameter
## getMatrix() - Return the matrix 
## setInverse() - Set the inverse of the matrix
## getInverse() - Return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize inverse of the matrix to NULL
        xInv <- NULL
        
        ## Function to set the matrix x to the one passed in parameter y
        setMatrix <- function(y) {
                ## If x and y are identical then nothing to do here.
                ## Else set x to y and clear the cached inverse.
                ## We need to clear the cached inverse when the matrix
                ## changes so that it will get re-computed in cacheSolve()
                if (!identical(x, y)) {
                        x <<- y
                        xInv <<- NULL
                }
        }
        
        ## Function to return matrix x
        getMatrix <- function() x
        
        ## Function to set the inverse of the matrix x
        setInverse <- function(inverse) xInv <<- inverse
        
        ## Function to return the inverse of the matrix
        getInverse <- function() xInv
        
        ## Return the special matrix which is really a list containing
        ## above functions
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve() - Computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## and the matrix has not changed, then it returns the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Get the inverse and check if it is NULL.
        ## If the inverse is not NULL then it indicates that
        ## the inverse has already been calculated. It also indicates that
        ## the matrix has not changed since the last time the inverse
        ## was calculated.
        ##
        ## Return the cached inverse.
        xInv <- x$getInverse()
        if(!is.null(xInv)) {
                message("Returning Cached Inverse of the Matrix")
                return(xInv)
        }
        
        ## We are here because either the inverse was never calculated or 
        ## the matrix has changed since 
        ## the last time it was calculated.
        ## Calculate the inverse of matrix x
        data <- x$getMatrix()
        xInv <- solve(data, ...)
        
        ## Cache the inverse
        x$setInverse(xInv)
        
        xInv
}
