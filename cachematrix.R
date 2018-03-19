## Put comments here that give an overall description of what your
## functions do
## This code contains a pair of functions that cache the inverse of a matrix

## Write a short comment describing this function
## Assuming an invertible matrix, this function creates a special 
## "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

# Checking if the provided input is an actual matrix
        if (!is.matrix(x)) {
           stop("You need to provide a matrix as input. Please change input and retry.")
                }
        
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
                }
        
        get <- function() x
        set_inv <- function(inverse) inv_mat <<- inverse
        get_inv <- function() inv_mat
        list(
                set = set,
                get = get,
                set_inv = set_inv,
                get_inv = get_inv)
        
}


## Write a short comment describing this function
## The following function (cacheSolve) computes the inverse of the special "matrix" given by
## the makeCacheMatri x. If the inverse has already been calculated, and there's no change
## in the matrix, then the cacheSolve() returns the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat <- x$get_inv()
        if(!is.null(inv_mat)) {
                message("Retrieving the cached inverse matrix.")
                return(inv_mat)
                }
         temp_mat <- x$get()
           inv_mat <- solve(temp_mat, ...)
           x$set_inv(inv_mat)
           inv_mat
           
}
