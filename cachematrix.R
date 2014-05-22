## This file conains two functions: makeCacheMatrix and cacheSolve. 
## By working together, these two functions can calculate the inverse matrix of the 
## input matrix and store the inverse matrix in the cache. After the first time of 
## the calculation using these functions, if the input matrix remains the same, 
## the code retrieves the inverse matrix from the cache.

## The makeCacheMatrix can cache the inverse matrix of the input matrix. 
## The input of makeCacheMatrix is a square invertible matrix. The output of makeCacheMatrix
## is a list of four functions: set, get, set_inverse_matrix, and get_inverse_matrix, 
## which can set the input matrix, get the input matrix, set the inverse matrix, and get the 
## inverse matrix, respectively.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverse_matrix <- function(IMatrix) m <<- IMatrix
        get_inverse_matrix <- function() m
        list(set = set, get = get,
             set_inverse_matrix = set_inverse_matrix,
             get_inverse_matrix = get_inverse_matrix)
}


## cacheSolve can calculate the inverse matrix of the input matrix in makeCacheMatrix 
## or retrieve the inverse matrix from the cache if it has been calculated last time. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse_matrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        IMatrix <- solve(data, ...)
        x$set_inverse_matrix(IMatrix)
        IMatrix
        
}

### Example:

### Create a 6X6 matrix
## xx <- rbind(c(9,-4,3,50,5,9),c(34,4,12,8,5,9),c(56,4,3,5,88,-15),c(500,45,7,2,0.5,5),
##            c(34,123,21,10,45,67),c(85,12,-2,-14,-45,59))

### Execute the functions
## a <- makeCacheMatrix(xx)
## cacheSolve(a)

### Execute cacheSolve again to see the difference in the output information
## cacheSolve(a)