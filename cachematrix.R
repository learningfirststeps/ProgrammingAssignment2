## "Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## [below is] a pair of functions that cache the inverse of a matrix"
## *) quote from the assignment

## Usage: 
## > source("cachematrix.R")            ## Source this file from working dir.
## > myCacheMatrix <- makeCacheMatrix() ## Creates a list
## > myCacheMatrix$set(matrix(1:4,2,2)) ## Assigns a matrix
## > cacheSolve(myCacheMatrix)          ## Inverse matrix is calculated
##                                         and stored to myCacheMatrix$m
##                                         environment cache
## > cacheSolve(myCacheMatrix)          ## From second time and on the inverse
##                                         matrix is taken from myCacheMatrix$m
##                                         environment cache 
## > myCacheMatrix$set(matrix(2:5,2,2)) ## Unless a new matrix is assigned

## "makeCacheMatrix: This function creates a special "matrix" [list] object
## that can cache its inverse." *) quote from the assignment

makeCacheMatrix <- function(x = matrix()) {
        ## makeCacheMatrix$m is used to save in cache inversed matrix
        m <- NULL
        
        ## makeCacheMatrix$set is used to assign matrix first time
        set <- function(y) { 
                x <<- y
                m <<- NULL
        }
        
        ## makeCacheMatrix$get is used to return the assigned matrix
        get <- function() x 
        
        ## makeCacheMatrix$setsolve is used to cache computed inverse matrix
        setsolve <- function(solve) m <<- solve
        
        ## makeCacheMatrix$getsolve returns cached inverse matrix
        getsolve <- function() m
        
        ## makeCacheMatrix returns a list of functions set above
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## "cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed),then the cachesolve should
## retrieve the inverse from the cache." *) quote from the assignment

cacheSolve <- function(x, ...) {
        ## x is a list created by makeCacheMatrix()
        
        ## First check if inverted matrix was stored in x environment before
        ## and return it from cache upon availability
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m) ## Return matrix that's the inverse of 'x' from cache
        }
        
        ## otherwise get the matrix from x environment, compute inverse one,
        ## store it to cache and return it
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setsolve(m)
        m               ## Return the computed matrix that's the inverse of 'x'
}