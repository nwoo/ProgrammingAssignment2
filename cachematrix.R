## The makeCacheMatrix function is a function that accepts x as an argument 
## and stores a list of functions. 
## It consists of 4 functions: set, get, setinverse, getinverse.
## x should be an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        # m is a variable that will be used to store the inverse of x
        
        m <- NULL
        
        # set is a function that changes the matrix stored in makeCacheMatrix
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # get is a function that returns the matrix x stored in makeCacheMatrix
        # and does not require any input.
        
        get <- function() x
        
        # setinverse is a function that simply stores the value of the solve function
        # into m
        # The solve function is used to solve for the inverse of object x        
        
        setinverse <- function(solve) m <<- solve
        
        # getinverse is a function that returns m
        
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the special "matrix" returned by
## the makeCacheMatrix function

## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve function should retrieve the inverse from the cache

cacheSolve <- function(x,...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
