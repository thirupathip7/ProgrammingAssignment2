## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {   
                x <<- y
                m <<- NULL # set the inverse of the Matrix
        }
        get <- function() x  # get the inverse of the Matrix
        setinverse <- function(inverse) m <<- inverse    # set the inverse of the matrix
        getinverse <- function() m    # get the inverse of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()  # get the inverse of the matrix
        if(!is.null(m)) {    #if inverse of matrix is exit,then retruns message "getting cached inverse of matrix" and returns inverse of matrix
                message("getting cached inverse of matrix")
                return(m)
        }
        data <- x$get() 
        m <- solve(data, ...)  # calculating inverse of matrix
        x$setinverse(m)
        m 
}
