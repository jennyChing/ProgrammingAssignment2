## Put comments here that give an overall description of what your
## functions do

## this function stores the inverse of the matrix x, taking x as input

makeCacheMatrix <- function(x = matrix()) {
        ## store the value of matrix inverse as NULL
        martrixInverse <- NULL
        ## define new function to set matrix x to a new matrix y and reset the matrix to NULL
        set <- function(y) {
                ## Create matrix for the first time
                x <<- y
                ## Reset the inverse matrix value in case the matrix is changed
                martrixInverse <<- NULL
        }
        ## new function get the value of the matrix x
        get <- function() x
        ## calculate the inverse of matrix x with solve function
        setinverse <- function(solve) martrixInverse <<- solve
        ## get the inverse value
        getinverse <- function() martrixInverse
        ## pass the value within the function makeCacheMatrix
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)

}


## this function get the cache of the inversed matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        martrixInverse <- x$getinverse()
        if(!is.null(martrixInverse)) {
            message("getting inverse matrix")
            return(martrixInverse)
        }
        data <- x$get()
        martrixInverse <- solve(data, ...)
        x$setinverse(martrixInverse)
        martrixInverse
}
