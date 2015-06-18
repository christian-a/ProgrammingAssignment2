## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. This pair of functions cache the inverse of a matrix (and the
## matrix itself) so it is not necessary to compute the inverse repeatedly.

#' Create a cached enabled matrix
#' The cache enabled matrix supports the following functions 
#' set, get, setsolve and getsolve
#' 
#' @param x A matrix.
#' @return A list with four functions to interact with the cached matrix
#' 
makeCacheMatrix <- function(x = matrix()) {
    ## cv stands for cached value
    cv <- NULL ## initial cv with null
    
    #' Sets a new matrix as cached enables matrix
    #' resets the cached value
    #' 
    #' @param y A matrix
    #' 
    set <- function(y) {
        if (is.matrix(y)) { ## check for matrix
            x <<- y ## stores the matrix
            cv <<- NULL ## reset cv to null    
        } else {
            warning("Not a matix")
        }
    }
    
    #' Returns the matrix
    #' 
    #' @return The matrix
    #' 
    get <- function() {
        x ## the matrix
    }
    
    #' caches the inversion of the matrix
    #' 
    #' @param solve The inversion of the matrix
    #' 
    setsolve <- function(solve) {
        cv <<- solve ## set cv as solve
    }
    
    #' Return the cached value
    #'
    #' @return The cached inversion of the matrix or null
    #' 
    getsolve <- function() {
        cv
    }
    
    ## Return a list with the four functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

#' Solve the cached matrix. If the matrix was already solved the cached value
#' is returned. Otherwise the matrix is solved and the result is cached inside  
#' the cached matrix
#' 
#' @param x A cached matrix (produced by makeCacheMatrix)
#' @param ... additional parameters for the solve function
#' @return The inversion of the cached matrix respectively the cached value
#' 
cacheSolve <- function(x, ...) {
    m <- x$getsolve() ## try to get the cached result
    if(!is.null(m)) { ## test for a cached result
        message("getting cached data")
        return(m) ## returns the cached result
    }
    
    data <- x$get() ## gets the unmodified matrix from the cached matrix
    m <- solve(data, ...) ## solves the matrix with the additional parameters
    
    x$setsolve(m) ## stores the result of solve in the cached matrix
    
    m ## returns the result
}