## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can be cached its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getinverse = getinverse)

}


## This function computes the inverse of the "matrix" created by above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
