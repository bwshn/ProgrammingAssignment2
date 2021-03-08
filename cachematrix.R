## Caching the inverse of a matrix

## It is a function to cache the inverse of a matrix rather than
## compute it repeatedly

## In the first matrix, creating special function that sets and gets the matrix;
## sets and gets the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y ## <<- searches for the all environments to find the variable
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return a matrix that is the inverse of 'x'

## In this function, the matrix is inverted, if it is solved before, uses the cached data

cacheSolve <- function(x, ...) {

    i <- x$getinv()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    matr <- x$get()
    i <- solve(matr, ...)
    x$setinv(i)
    i
}

## I honestly should tell, I used the "mean" example, almost copied it.
