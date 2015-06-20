## the functions help in finding the inverse of a
## matrix. If the inverse is already computed it
## uses the cached value.

## makeCacheMatrix gives a list of functions to 
## set and get the matrix and it's mean. This
## can be used to cache the values

makeCacheMatrix <- function(x = matrix()) {
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


## computes the inverse of a matrix. Uses the
## cached value if already computed.

cacheSolve <- function(x, ...) {
         i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
