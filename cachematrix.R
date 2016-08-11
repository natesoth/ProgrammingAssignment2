## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        xInv <- NULL
        setMat <- function(y) {
                x <<- y
                xInv <<- NULL
        }
        getMat <- function() x
        setInv <- function(inverse) xInv <<- inverse
        getInv <- function() xInv
        list(setMat = setMat, 
             getMat = getMat,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xInv <- x$getInv()
        if(!is.null(xInv)) {
                message("getting cached inverse matrix...")
                return(xInv)
        }
        mat <- x$getMat()
        xInv <- solve(mat)
        x$setInv(xInv)
        xInv
}
