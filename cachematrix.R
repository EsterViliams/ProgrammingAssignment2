## First function will be used for implementing a way to calculate inverse 
## of a matrix, and the second function will be used to return (and calculate 
## if needed) cached value of that inverse matrix.

########

## This function sets and gets value of matrix, and sets and gets inverse of
## the same matrix. For calculating inverse of matrix is used function
## solve(a, b, ...) where b is a numeric or complex vector or matrix giving 
## the right-hand side(s) of the linear system. Since it is missing, b is taken 
## to be an identity matrix and solve will return the inverse of a.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve  #function solve calculates inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function will first get the value of inverse matrix and check if the  
## inverse of matrix 'x' has already been calculated. If we can get cached data 
## (inv is not null), function will return that cached data 'inv' and function 
## will end there. 
## Otherwise, if cashed data is null, this function will first get matrix,  
## next calculate inverse, after that set inversed matrix as cach, 
## and finally return cached inverse matrix 'inv'.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)                     # returning cached inverse matrix
    }
    data <- x$get()                     # getting matrix 'x'   
    inv <- solve(data,...)              # calculating inverse matrix 'inv'
    x$setinverse(inv)                   # setting cache of inverse matrix
    inv                                 # returning cached inverse matrix
}