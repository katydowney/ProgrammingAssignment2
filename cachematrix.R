## Caching the inverse of a matrix.
## The functions below cache the inverse of a matrix.


## makeCacheMatrix creates a special matrix data structure
## that can store the inverse of the matrix. The function
## sets and gets the values of the matrix, and sets
## and gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    ## set matrix function
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    ## get matrix function
    get <- function() x
    ## set inverse matrix function
    setInverseMatrix <- function(iMatrix) inverseMatrix <<- iMatrix
    ## get inverse matrix function
    getInverseMatrix <- function() inverseMatrix
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverseMatrix()
    ## if the matrix is not NULL then we will return it
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    ## the matrix is NULL so we will calculate the value
    data <- x$get()
    inverseMatrix <- solve(data)
    ## set the inverse matrix in x and return the inverse
    x$setInverseMatrix(inverseMatrix)
    inverseMatrix
}
