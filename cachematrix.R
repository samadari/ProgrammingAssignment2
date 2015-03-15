
## Functions to compute the inverse of a matrix and save it in the cache 
## so next time the inverse of the matrix is needed it's faster to get


## Function to create an object associated to a matrix; it contains a list with
## a function to set the value of the matrix (and its inverse to NULL) in cache
## a function to get the value of the matrix
## a function to set the value of the matrix inverse in cache
# a functiojn to get the value of the matrix inverse from cache

makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL
        # set the values of the matrix and its inverse (NULL) in cache
        setmatrix <- function(y) {
                mcache <<- y
                inv <<- NULL
        }
        # get the matrix
        getmatrix <- function() m
        # set the value of the matrix inverse in cache
        setinv <- function(auxinv) inv <<- auxinv
        # get the matrix inverse from cache
        getinv <- function() inv
        list(setmatrix = setmatrix, getmatrix = getmatrix,
                setinv = setinv,
                getinv = getinv)
}


## Function to calculate the inverse of a matrix m given as parameter
## It first checks whether the inverse matrix is already in cache

cacheSolve <- function(m, ...) {
        # get the inverse matrix from cache
        inv <- m$getinv()
        # check whether inv is stored in cache
        if(!is.null(inv)) {
                # if so, return the cached value
                message("getting cached matrix inverse")
                return(inv)
        }
        # if not, compute and store it
        mat <- m$getmatrix()
        inv <- solve(mat)
        m$setinv(inv)
        inv
}

