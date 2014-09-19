## As part of the Second programming assignment for the R Programming Coursera course:
## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
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

## The following function, cacheSolve, calculates the inverse of the special "matrix" created with the above function, makeCacheMatrix.
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setsolve function.
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setsolve(m)
        m
}

## Use case:
## 1. Create the special "matrix" vector using: z <- makeCacheMatrix(x) ## where x is an existing inversable matrix
## 2. Calculate the inverse of the special "matrix" using: cacheSolve(z) ## if the inverse is cached a message will be display (getting cached data) as well as the inverse matrix