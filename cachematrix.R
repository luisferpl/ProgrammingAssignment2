##Matrix inversion is usually a costly computation and there may be some 
##benefit to caching the inverse of a matrix rather than compute it repeatedly 
##(there are also alternatives to matrix inversion that we will not discuss here). 
## This pair of functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inver_x <- NULL
        set <<- function(y) {
                x <<- y
                inver_x <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver_x <<- inverse
        getinverse <- function() inver_x
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver_x <- x$getinverse
        if(!is.null(inver_x)){
                message("getting cached inverse matrix")
                return(inver_x)
        } else {
                inver_x <- solve(x$get())
                x$setinverse(inver_x)
                return(inver_x)
        }        
}
