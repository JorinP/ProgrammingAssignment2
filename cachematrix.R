## This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
        cacheinverse <- NULL
        set <- function(y) {
                x <<- y
                cacheinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) cacheinverse <<- inverse
        getinverse <- function() cacheinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        cacheinverse <- x$getinverse()
        if(!is.null(cacheinverse)) {
                message("getting cached data")
                return(cacheinverse)
        }
        data <- x$get()
        cacheinverse <- solve(data)
        x$setinverse(cacheinverse)
        cacheinverse
}
