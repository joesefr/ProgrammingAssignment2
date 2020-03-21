## These functions are used to caching the inverse of a matrix

## This funcion creates a special matrix onject thath can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    New <- function(y){
      
        x <<- y
        inv <<- NULL
    }
    
    Get <- function()x
    
    SetInverse <- function(inverse) inv <<- inverse
    GetInverse <- function() inv
    list(New = New, Get = Get, SetInverse = SetInverse, GetInverse = GetInverse)
    
}


## This function computes the inverse of the special matrix returned by 
## the function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$GetInverse()
    
    if(!is.null(inv)){
      message("Retrieving cached data")
      return(inv)
    }
    
    Data <- x$Get()
    inv <- solve(Data, ...)
    x$SetInverse(inv)
    inv
}
