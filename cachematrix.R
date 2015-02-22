## These functions calculate the inverse of a matrix
## They allow the result to be cached, to avoid recalculation if the result is already stored

## The function makeCacheMatrix creates a list of functions to set and get the value of the matrix
## and the value of the inverse of the matrix

makeCacheMatrix <- function(x = array()) {
        ## Define inverse and clear value
        inverse <- NULL
        
        ## Function to set x to be the value of an exisitng definition of y from a parent environment
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ## Get the value of x
        get <- function() x
        
        ## Set the value of the inverse of the matrix
        setinverse <- function(solve) inverse <<- solve
        
        ## Get the value of the inverse of the matrix
        getinverse <- function () inverse
        
        ## Return a list of all the define functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve checks if the value of the inverse of the matrix has already been calculated.
## If it has, it gets the inverse from the cache, otherwise it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        
        ## Check if the inverse has alrady been calculated
        if(!is.null(inverse)) {
                message("getting cached data")
                
                ## Return the already calculated value
                return(inverse)
        }
        data <- x$get ()
        
        ## Calculate and return the inverse of the matrix
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}


