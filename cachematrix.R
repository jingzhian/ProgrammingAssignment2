## This series of functions enables the user to cache potentially time-consuming matrix inversions. 
## This is as a part of Coursera R Programming Assignment 2

## The first function creates an special matrix object that stores a matrix and cache's its inverse. It is really a list, containing 1. set the value of the matrix, 2) get the value of the matrix 3) set the value of the inversion 4) get the value of the inversion

makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL
        setMatrix <- function (y) {
                x <<- y
                ix <<- NULL
        }
        getMatrix <- function() x
        setinverseMatrix <- function(inverseMatrix) ix <<- inverseMatrix
        getinverseMatrix <- function() ix
        list(setMatrix = set, getMatrix = get, setinverseMatrix = setinverseMatrix, getinverseMatrix = getinverseMatrix)
}


## This second function takes in a special matrix as created by the first function and calculates the inverse of the matrix. It first checks to see if the inverse has already been computed. If so, it 'get's the mean from the cache and skips the computation. Otherwise it calculates the inverseMatrix and sets the inverseMatrix in the cache via the 'setinverseMatrix' function. Note: in the implementation we have assumed that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ix <- x$getinverseMatrix()
        if (!is.null(ix)){
                message("getting cached matrix")
                return(ix)
        }
        # Otherise get the matrix,calculate the inverse and store it in the cache
        matrix <- x$getMatrix()
        ix <- solve(matrix, ...)
        x$setinverseMatrix(ix)
        ix
}
