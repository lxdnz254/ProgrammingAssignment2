## These two functions will create a "special" matrix as defined by the user.
## It will store the matrix and cache its inverse.

## makeCacheMatrix makes a "special" matrix, which is really a list that
## 1. sets the matrix
## 2. gets the matrix
## 3. sets the inverse
## 4. gets the inverse

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


## cacheSolve will calculate the inverse of the "special" matrix
## created in makeCacheMatrix, but first it will check if the inverse
## has already been calculated. If so, it gets the inverse from the cache
## and skips the computation. Otherwise it will calculate inverse and then set
## the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m       ## Return a matrix that is the inverse of 'x'
}
