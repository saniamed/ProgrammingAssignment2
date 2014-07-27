## Assignment 2
## github username: Saniamed  


## This function creates a matrix object that cache's its inverse
## 
## After a cache-matrix has been created by calling this function
## cacheSolve() can be called to compute and cache the inverse
##
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(snew) s <<- snew
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## This function returns the inverse of cache-matrix
##      Note: A cache-matrix is created by calling makeCacheMatrix()
## It first checks the cached inverse of the matrix & returns
## that if found...
## else it calculates the inverse, saves it in the cache, and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        snew <- solve(data, ...)
        x$setsolve(snew)
        snew
}
