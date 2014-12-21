## Functions makeCacheMatrix and cacheSolve work in unison to
## cache a matrix and the inverse of a atrix, and to compute
## the inverse of the matrix passed in.  makeCacheMatrix caches
## the results of the inverse and the matrix itself, cacheSolve
## computes the inverse of the matrix and stores it in the cache

## makeCacheMatrix returns a list of 4 functions to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix
## the matrix is cached in the variable x and
## the inverse is cached in the variable i.
## We use the <<- to assign x and i in a different environment
## than the environment of this function, this way the get and set
## functions can be used in other funcions.  The matrix
## to be cached can be passed into the function or set
## with the set function.  If the set functions is used the function
## will store the matrix in x and clear out the inverse i.
makeCacheMatrix <- function(x = matrix()) {
    ## 'x' is a matrix that can be passed in to be cached, the default
    ## is the empty matrix
    
    ## return a list of 4 functions to get, set the matrix and it's
    ## inverse
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special matrix (the
## cached matrix).  If the inverse has already been computed (determined
## by the getinverse function) then cacheSolve simply returns the 
## cached inverse.  If it needs to compute the inverse it uses the 
## setinverse function to cache the inverse it computed.
cacheSolve <- function(x, ...) {
    ## 'x' is the special matrix (i.e. the cache of the matrix)
    
    ## Return a matrix that is the inverse of 'x'
    
    i = x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}