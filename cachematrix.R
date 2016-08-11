#### Week 3 Assignment: Caching inverted matricies ####

## This set of functions provides an analogous functionality to that provided
## by the original cache mean functions. The difference between the two sets
## of functions is that this set it used for inverting and cacheing matricies,
## while the original two functions were used for calculating the mean of 
## vectors. The union of the two functions allows the user to define a matrix,
## compute the matrix's inverse, and then store the results in the function's
## parent environment. The stored result can then be returned by either calling
## the cacheSolve function on the special list created by the makeCacheMatrix
## function, or it call be returned with the `specialObject$getInv()` function.
## For large invertible matricies, being able to store a matrix's inverse after
## computing it is both efficient and time saving.


#### Function 1: makeCacheMatrix ####

## The makeCacheMatrix function provides the framework within which lists
## containing matricies and their inverses can be defined, stored and returned. 
## The object returned by the makeCacheMatrix is a special list of functions. 
##
## The first element in the list is a funciton called `setMat`, which is used
## for storing a matrix as x in parent environ if a matrix hasn't already been
## stored. Alternatively, the user can use the `setMat` function in the same way
## to overwrite a previously stored matrix (and inverse matrix if it exists). 
## It is important to remember that overwriting the stored matrix will reset the
## xInv object in the environment to NULL, which could be a costly mistake if 
## calculating the inverse of the matrix was time consuming. Note that the 
## makeCacheMatrix function can also be called with a matrix as it's argument 
## to avoid having to use the `setMat` function.
##
## The second element of the special list created by the makeCacheMatrix 
## function is a function called `getMat`. The getMat funciton simply returns
## the matrix that has already be stored in the parents directory by the 
## function's initial call, or by using the setMat function. If no matrix has
## yet been stored, then the getMat will return a 1x1 matrix with the value NA.
##
## The third element is the function setInv. This function allows the cacheSolve
## function to store the inverted matrix that it creates within the special list
## created by the makeCacheMatrix function.
##
## Finally the fourth element functions the same as the second, however getInv
## will return the inverse matrix that was stored by calling the cacheSolve 
## function with the special list object as its argument. If the cacheSolve 
## function has not yet been called with the special object at its arg then
## this function will return the null value that was assigned xInv in the parent
## environment during the makeCacheMatrix's original call.


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

#### Function 2: cacheSolve ####

## The cacheSolve function is largely just the implementation of the functions
## defined as elements in the special list created by makeCacheMatrix. 
## 
## The function first pulls the xInv object from the special list and stores is 
## as xInv in its own environment. Then the contents of xInv are checked, if the
## cacheSolve function has been called with the special list before, then xInv
## will NOT be NULL and we will enter the block of code within the IF statement. 
## The the messaged will be printed, and the inverse matrix that was brought 
## from the special list and stored in xInv is returned. 

## If xInv is NULL then we skip the block of code and call the special object's
## getMat function to store the matrix in the mat object. This mat object is 
## then inverted and stored in the xInv object, and the inverted matrix is then 
## passed back to and stored in the special list's environment using the setInv 
## function. Finally the cacheSolve function returns the xInv object, which is
## the inverse of the matrix that was stored using the makeCacheMatrix.

## Now if the cacheSolve function is called again, the matrix doesn't need to be 
## recalculated, instead it is brought in the from the special list's 
## enivronment and returned after the message "getting cached inverse matrix..."
## Lastly, if the specialList object is called like so `specialListOb$getInv()`,
## the inverted matrix will be returned.

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
