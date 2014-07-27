## Assignment 2
## github username: Saniamed  
##
##
##


## This function creates a matrix object that caches its inverse
##    I will refer to this matrix object as a cache-matrix
## 
## After a cache-matrix has been created by calling this function
## cacheSolve() can be called to compute and cache the inverse
##
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        # set the data matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        # return the data matrix
        get <- function() x
        
        # set the cached-inverse of the matrix
        setsolve <- function(snew) s <<- snew
        
        # get the cahced-inverse of the matrix
        getsolve <- function() s
        
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## This function returns the inverse of cache-matrix
##      Note: A cache-matrix is created by calling makeCacheMatrix()
## It first checks the cached inverse of the matrix & returns
##    that if found...
## else it calculates the inverse, saves it in the cache, and returns it
##
cacheSolve <- function(x, ...) {
        ## check cached-inverse first
        s <- x$getsolve()
        
        # do we have a proper cahced inverse?
        if(!is.null(s)) {
                message("getting cached data")
                return(s) # return cached inverse
        }
        
        # must calculate inverse as cache is null
        data <- x$get()
        snew <- solve(data, ...)
        x$setsolve(snew) # save newly calculate inverse in cache
        snew
}
