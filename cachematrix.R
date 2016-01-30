## These functions create a special matrix capable of
## cacheing the inverse of itself and returning the 
## cached inverse when it is valid to do so (when the
## inverse has already been cached and the input matrix
## has not been changed)

## This function builds the special matrix and creates the
## get and set methods that it uses to implement cacheing 
## for its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks to see if there is a cached inverse.
## If so, that value is returned. If not, the inverse is calculated
## and it is entered into the cache AND returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("Retrieving cached value")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
