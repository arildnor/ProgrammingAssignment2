################################################################################
##
## Using global assignment <<- to set up an environment that can be used 
## to invert matrixes using the solve function. The <<- make it possible to keep
## in memory (not re initialized) when calling the function again. This
## could potentially be time saving for computing on large matrixes. The 
## downside is the fact that this makes the code less readable / understandable.
## Some R people actually warn against the <<- operator and some even want to
## remove it completely. This coding is based on the understanding and 
## experimenting with the makeVector / cachemean example given.
## 
## This is the function setting up the environment (cache) to be used. 
## The <<- operator does the caching.
#
makeCacheMatrix <- function(x = matrix()) {
    #
    # There are no Matrix Inverse for now ?
    #
    matrixInverted <- NULL
    #
    # For inspections
    #
    #print(environment())
    #evn <- environment()
    #print(parent.env(evn))
    set <- function(y){
        #
        # Make x a global variables. Can be accessed outside the function
        #
        x <<- y
        matrixInverted <<- NULL
    }
    ## Here getting the value of the inverse
    get <- function() x
    setinverse <- function(solve) {
        matrixInverted <<- solve
        #
        # Telling me it is actually solving here
        #
        print("Calculating the Inverse Matrix")
    }
    ## Get back the inverted matrix from cache
    getinverse <- function() matrixInverted
    ## This list of functions are returned from the function
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}
################################################################################
##
## Want to get the cached inverse matrix back if it is already calculated
## If not it will be calculated on first round.
## From my test setup below I see that first round calculates the inverse.
## Second round picks it from the cache
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # The inverted matrix is set as global named x
    matrixInverted <- x$getinverse()
    ## Need to see if inverse exist already
    if(!is.null(matrixInverted)) {
        # Telling me it is using the already solved matrix in the cache
        print("Getting cached Inverse Matrix Data")
        return(matrixInverted)
    }
    # No Inverted matrix exist will calculate it and get the inverted back
    data <- x$get()
    matrixInverted <- solve(data, ...)
    x$setinverse(matrixInverted)
    matrixInverted
}
################################################################################
#
#
# Test with a small solvable matrix
#
#test_matrix <- diag(nrow=100,ncol=100)
test_matrix <- matrix(c(4, 3,3, 2),nrow=2,ncol=2) 
#print(test_matrix)
#
# Setting up and returning the environment
#
matxenvironment <- makeCacheMatrix(test_matrix)
print(matxenvironment)
#
# Solving the matrix put into the environment
# Set up to check the time used to see what to save.
#
start.time <- Sys.time()
inverseMatrix <- cacheSolve(matxenvironment)
#print(inverseMatrix)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

