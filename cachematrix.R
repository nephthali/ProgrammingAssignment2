## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        is.square <- function(M){
                return( is.matrix(M) && (nrow(M) == ncol(M)))
        }
        if(is.square(data)){
                #message("execution of solve function")
                inv <- solve(data)
        } 
        
        #else {
        #message("execution of ginv function")
        ## library MASS must be load to get the Generalized Inverse of Matrix (ginv) function
        #inv <- ginv(data)    
        #}
        x$setinv(inv)
        inv
}
