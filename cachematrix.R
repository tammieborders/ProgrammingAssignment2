makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    # Sets value of matrix
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    
    # Gets the value of the matrix
    get <- function() x
    
    # Sets value of matrix inverse
    setinverse <- function(inverse) m <<- inverse
    
    # Gets values of matrix inverse
    getinverse <- function() m
    
    # list that contains functions to set, get, setinverse, getinverse
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    # Checks if inverse exists, uses cached data
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # Inverse did not exist, calculates inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}