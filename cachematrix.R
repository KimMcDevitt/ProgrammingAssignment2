## Programming Assignment 2
## Write a pair of functions to cache the inverse of a matrix

## This function sets the value of the vector and sets the mean.  It also gets the value and the mean

makeVector <- function(x = matrix()) {
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

## calcuates the mean of hte special vecotr created from the makeVector function.  It checks to see if it exists and it if does
## returns that value otheriwse it calculates the mean.  
cachemean <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

## This function creates a special matrix object that can cache it's inverse
makeCacheMatrix <- function(x){
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, 
                setsolve = setsolve,
                getsolve = getsolve)
}

## This function computes the inverse of the special matrix returned by makeCacheMatrix.  If the inverse has already been calculated
##  (and the matrix has not changed), then the cachesolve should retrieve the matrics from the cache.  
cacheSolve <- function(x=matrix(), ...){
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
## create a square matrix becuase solve only handles square matricies

x <-matrix(sample(16,16,T),4,4)
a <- makeCacheMatrix(x)
a$get()
a$getsolve()
cacheSolve(a)
