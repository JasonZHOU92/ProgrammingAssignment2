 
## These two functions calculate the inversion of a Matrix and cache the result


## MakeCacheMatrix() creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set<- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    
    setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
    getInverse <- function() inverse
    list(set=set, get=get, setInverse, getInverse)
}


## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    data<-x$get()
    inverse <- ginv(data, ...)
    x$setInverse(inverse)
    inverse
}
