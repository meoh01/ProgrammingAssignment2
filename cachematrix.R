## Creates a 'solve' value and caches it.  When run again, it checks to see if there's a cached value,
##   if not, it creates the 'solve' value and caches it

## This function checks to see if the 'solve' value was cached, if not it calls 'cachSolve.R'
##   to create the 'solve' value and caches it

makeCacheMatrix <- function(x = matrix()) {
        s1 <- NULL
        set <- function(y) {
                x <<- y
                s1 <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s1 <<- solve
        getsolve <- function() s1
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Checks to for an existing 'solve' value, if none, it creates one and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s1 <- x$getsolve()
        if(!is.null(s1)) {
                message("getting cached solve data")
                return(s1)
        }
        data <- x$get()
        s1 <- solve(data, ...)
        x$setsolve(s1)
        s1
}