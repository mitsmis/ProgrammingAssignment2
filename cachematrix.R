## Matrix inversion is a costly endeavor.
## Because of this, there is a benefit to caching the inverse of
## a matrix, as opposed to recalculating from scratch. 

## The makeCacheMatrix funtion checks to see if resources have already
## been used to calculate the inverse and will skip the calculation 
## if it has already been completed.
## makeCacheMatrix is a function that creates a matrix that "can" cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        ## Chose "inv" as appreviation to inverse for variable.
        ## I did this so the function calls were more intuitive,
        ## as recommended per coding styling rules.
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve is a function that solves the cache when it is not present to begin with
## when first trying to get the Inverse using the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("fetching cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setInverse(inv)
        inv
}

