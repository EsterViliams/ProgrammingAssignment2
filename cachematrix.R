## First function will be used for implementing a way to calculate inverse 
## of a matrix, and the second function will be used to return (and calculate 
## if needed) cached value of that inverse matrix.

### This function sets and gets value of matrix, and sets and gets inverse of
### the same matrix.

makeCacheMatrix <- function(x = matrix()) {
    c <- NULL
    set <- function(y) {
        x <<- y
        c <<- NULL
    }
    get <- function() x
    inverse<-function(){
        ...
    }
    setinverse <- function(inverse) c <<- inverse ##calculate inverse
    getinverse <- function() c
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


### This function will first check if the inverse of matrix 'x' has already
### been calculated. If we can get cached data (c is not null), function
### will return that cached data 'c'. Otherwise, if cashed data is null, this 
### function will first get matrix, next calculate inverse, after that set 
### inversed matrix as cach, and finally return cached inverse matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    c <- x$getinverse()
    if(!is.null(c)) {
        message("getting cached data")
        return(c)                     ### returning cached inverse matrix
    }
    data <- x$get()                   ### getting matrix    
    c <- inverse(data,...)            ### calculating inverse matrix
    x$setinverse(c)                   ### setting cache of inverse matrix
    c                                 ### returning cached inverse matrix
}