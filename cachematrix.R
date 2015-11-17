## R Programming - Assignment 2


## The function makeCacheMatrix creates a special matrix object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {         
        
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x
    setinv <- function(inverse) 
    inv <<- inverse 
    getinv <- function() inv
    list(set=set, 
         get=get, 
         setinv=setinv, 
         getinv=getinv)
}



## The function cacheSolve determines whether the inverse of a matrix previously
## used as the argument to makeCacheMatrix has already been calculated. 
## If so, cacheSolve retrieves its inverse from the cache. Otherwise, the solve
## function is used to get the inverse matrix of x.
 
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'   
    
    inv <- x$getinv()
        
    if (!is.null(inv)){
         message("retrieving the inverse from the cache")
         return(inv)
        }
        
        matrix <- x$get()
        inv <- solve(matrix, ...)
        
        x$setinv(inv)
        
        return(inv)
}