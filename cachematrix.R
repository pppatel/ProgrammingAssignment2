
## The first function, makeVector creates a special "vector", which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the inverse of a Matrix
## get the value of the inverse of a Matrix


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    get <- function() x
    getInverse <- function() i
    
    set <- function(matrix) {
        x <<- matrix
        i <<- NULL
    }    
    setInverse <- function(inverse) i <<- inverse 
    list (get = get, set = set,
          getInverse = getInverse, setInverse = setInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("Getting data from cache!")
        return(i)
    }
    
    i <- solve(x$get(), ...)
    x$setInverse(i)
    i
}
