## The functions in this file aim to provide a special type of matrix that can
## save its inverse in a cache memory. This is useful to optimize code in which 
## the inverse might be used several times, because it is computed only once, 
## and then recovered from the cache memory. If the matrix changes, the inverse
## is set to NULL, therefore, in the next call to get the inverse, it will be
## calculated again.

## This function creates a special type of matrix that can hold its inverse in a 
## cache memory. This special type of matrix is based on a list.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse  
    getinverse <- function() i
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## Returns the inverse of the matrix passed as parameter 'x'. 'x' should be an 
## instance of the special matrix created with the makeCacheMatrix function.
## The function first checks if the inverse was already calculated, if so, it
## returns the content of the cache memory, otherwise, it computes, stores and 
## returns the inverse matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)){
        message("Getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
