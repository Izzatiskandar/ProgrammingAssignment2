## The functions create a matrix and calculate the inverse of the
## created matrix.If the matrix has been calculated, it will retrieve
## them from the cache and skip the computation. Otherwise, it will
## calculate the inverse and set it in the cache.

## makeCacheMatrix creates a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
                

}


## cacheSolve calculates the inverse of the matrix created using
## the above function. If the inverse has been calculated, 
## it retrieve it from the cache and skips the computation.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
        ## Return a matrix that is the inverse of 'x'
}
