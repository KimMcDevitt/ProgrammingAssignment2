## Programming Assignment 2
## Write a pair of functions to cache the inverse of a matrix

## This function creates a special matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get, 
                setmean = setmean,
                getmean = getmean)

}


## This function computes the inverse of the special matrix returned by makeCacheMatrix.  If the inverse has already been calculated
##  (and the matrix has not changed), then the cachesolve should retrieve the matrics from the cache.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mean <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
x <- matrix(runif(1:100, 5,20), 10, 10)
makeXCache <- makeCacheMatrix(x)
makeXcacheSolve <- cacheSolve(x)
