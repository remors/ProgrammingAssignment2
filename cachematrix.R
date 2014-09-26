## Create a vector of matricese and their inverse along with functions to 
## add new matrices and store solved inverses
##
## Callable with cache <- makeCacheMatrix(matrix)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function returns the inverse to a matrix
## This is done by checking to see if that matrix already exists in the cache 
## and if so, return the inverse. If it doesnt, it solves it the first time
## and stores it in the cache for subsequent calls.
##
## Callable with cacheSolve(cache)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
