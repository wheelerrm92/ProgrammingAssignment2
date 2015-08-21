#These functions create a matrix class that can store both the starting matrix and its inverse

##makeCacheMatrix defines a cached matrix class. The class contains 4 functions that allow an initial matrix to be stored, set, and called upon
#further a inverse of a given matrix can be stored and called upon.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmatrix <-function(y){
        x <<- y
        inv <<- NULL}
    getmatrix <- function() x
    setinverse <- function(inverse) inv<<-inverse
    getinverse <- function() inv
    
    list( getmatrix = getmatrix,
          setmatrix = setmatrix,
          setinverse = setinverse,
          getinverse = getinverse)
    
    
}


## CacheSolve function requires a matrix of the cached matrix class, the function looks to see if an inverse has been stored
# if it has the inverse is called upon, if not the inverse is calculated then stored for future use

cacheSolve <- function(x=matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
        if(!is.null(inverse)==TRUE){
            message("getting cached data")
            return(inverse)
            }
        else{
            message("no cached data, solving for matix")
            inverse <- solve(x$getmatrix())
            x$setinverse(inverse)
            inverse
            }
    }
