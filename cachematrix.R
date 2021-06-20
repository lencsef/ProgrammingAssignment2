## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv_init  <- NULL
    set <- function(y){
        x <<- y
        inv_init <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv_init <<- inverse
    get_inverse <- function() inv_init
    list(set = set,
         get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse
    )
}


## Write a short comment describing this function
# Solving inverse of the matrix. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_init <-x$get_inverse()
        if (!is.null(inv_init)) {
            message("Cached inverse of matrix found. Getting cached matrix")
            return(inv_init)
        } 
        mat <- x$get()
        inv_init <- solve(mat,...)
        x$set_inverse(inv_init)
        inv_init
}
