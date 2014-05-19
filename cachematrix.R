## The following functions are used to create a special "matrix" object that 
## stores a square matrix and caches its inverse

## Creates a special "matrix" which is a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # function to set the value of x
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # function to set the value of x
    get <- function() x
    
    # function to set the value of inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # function to get the value of inverse
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of a special "matrix" x. In case the inverse is already
## cached, its value is simply returned. Otherwise, the inverse of x is computed
## and cached
cacheSolve <- function(x, ...) {
    # check if inverse matrix is already cached
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()    # get matrix value
    
    #check if matrix x is square
    nr = nrow(data);
    nc = ncol(data);
    if (nr!=nc) {
        stop("input matrix is not square!")
    }
    
    inv <- solve(data)     # calculate inverse matrix
    x$setinverse(inv)    # cache inverse matrix
    inv    # print inverse matrix 
}
