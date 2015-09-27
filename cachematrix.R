## The purpose of the overall program is to cache the inverse of a matrix
## so as to avoid repeated computation (As matrix inversion is usually a costly computation)
## The program comrpises of two functions makeCacheMatrix and cacheSolve defined below.

## Below function creates a special "matrix" object that can cache its inverse.
## This matrix is a list containing a function to set the value of the matrix, get the
## value of the matrix, set the value of the inverse and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    set <- function(y) 
    {
        ## Operator <<- is used to assign value to an object in environment different
        ## than current environment.
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## Below function returns inverse of the input matrix 'x'
## To do so, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from cache and doesn't do the computation.
## If not, it computes the inverse and sets it in the cache.

cacheSolve <- function(x, ...) {
        
    inv <- x$getinverse()
    
    # Check if inverse has been calculated and is in cache
    if(!is.null(inv)) 
    {
        message("Found cached inverse of matrix")
        return(inv)
    }
    
    # Else, caclulate the inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    # Set value of inverse in cache
    x$setinverse(inv)
    
    inv
}
