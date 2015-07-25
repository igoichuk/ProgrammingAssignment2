## Put comments here that give an overall description of what your
## functions do

## Fatory function that contains cached value of a calculation and getters/setters

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(invMatrix) m <<- invMatrix
    getInverseMatrix <- function() m
    
    list(
        set = set, 
        get = get,
        setInverseMatrix = setInverseMatrix,
        getInverseMatrix = getInverseMatrix
    )
}


## Returns cached inverse matrix or calculates and updates cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getInverseMatrix()
    
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setInverseMatrix(m)
    m
}
