## The following two functions create, calculate the inverse of th input matrix 
##and return the inverse matrix.


## the following function creates a matrix, sets the matrix, gets the matrix
##sets the inverse of the input matrix and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## the following function checks whether the inverse of the matrix is already calculated in the cache. 
##If it is, it returns the inverse Else the function gets the input matrix, calculates its inverse and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
