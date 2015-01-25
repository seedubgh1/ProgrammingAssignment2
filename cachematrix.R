## These functions use lexical scoping to manipulate a cached matrix object
## i.e., to cache the original and inverse of a matrix

## makeCacheMatrix returns a list of functions to be used on a matrix
## object.  The functions are set, get, setinverse and getinverse
## Sample useage:
## Create a matrix: mtx <- matrix(1:4,nrow=2, ncol=2)
## Create the cache functions: mtxlist <- makeCacheMatrix(mtx)
## Get the inverse: cacheSolve(mtxlist)

## If cacheSolve() is executed without changing the matrix, a message
## should be displayed showing that the cached version is being
## retrieved

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse<- function () m
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## cacheSolve computes the inverse of matrix object x
## if the inverse has already been computed, it will be retreived
## from the cache.  Otherwise the inverse will be computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}

