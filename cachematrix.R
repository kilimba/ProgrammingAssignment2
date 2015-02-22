## This code has 2 functions. The first one, makeCaheMatrix takes a matrix as an argument and returns a 
## list of functions which manipulate this matrix and its (cached) inverse. The second function, cacheSolve
## uses the accessor functions in makeCacheMatrix to check whether the inverse variable has ever been set.
## If it ever has, then it returns this cached version, if it has never been set, cacheSolve calculates this inverse
## and sets the result into the makeCacheMatrix object for future cached referencing.

## This following function takes a matrix and returns a list of functions which either set the matrix,
## get the matrix, set the inverse of the input matrix or return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of a matrix, a matrix which happens to have been created with
## the above function. It accepts a list of functions as its first argument. It uses one of those functions 
## to get the cached inverse of the matrix fed into makeCacheMatrix. Depending on whether this is the first time running 
## cacheSolve has been run, this inverse may not have been computed yet, and therefore, the cached inverse will be null
## and cacheSolve will go ahead and compute this inverse and eventually set the inverse to be cached for future faster
## reference

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
