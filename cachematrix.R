## Lori Rivers
## R Programming - Coding Assignment 2

## The makeCacheMatrix function takes the cacheSolve function as an object argument.  It includes getter and setter functions to capture values that are
## set in the child function, the cacheSolve function.  
## A third function, prodSold.R, produces a matrix containing the mean and inverse mean of cups used in  Company XYZ regional offices in the US and Canada.
## (Size of matrix is small because I'm using my company-issued computer)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## the cacheSolve function  is the function of passed as an argument to the makeCacheMatrix function.  It creates an inversed matrix.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

## Use the prodSold function to test the cachematrix function. 
prodSold = function(mat){
    cups = makeCacheMatrix(mat)
    
    cacheSolve(cups)
    sold <-array(rnorm(2,20,20), c(2,20,20))
    colnames(sold) <- c("EAST", "WEST", "NORTH", "SOUTH", "NORTHEAST", "NORTHWEST", "SOUTHEAST", "SOUTHWEST", "MIDWEST", "ONTARIO", "QUEBEC", "BRITISH COLUMBIA", "ALBERTA", "ALBERTA", "NOVA SCOTIA", "NEWFOUNDLAND", "SASKATCHEWAN", "MANITOBA", "NEW BRUNSWICK", "PRINCE EDWARD ISLAND")
    apply(sold,c(1,2), mean)
    print(sold)
    
    cacheSolve(cups)
    sold <-array(rnorm(2,20,20), c(2,20,20))
    colnames(sold) <- c("EAST", "WEST", "NORTH", "SOUTH", "NORTHEAST", "NORTHWEST", "SOUTHEAST", "SOUTHWEST", "MIDWEST", "ONTARIO", "QUEBEC", "BRITISH COLUMBIA", "ALBERTA", "ALBERTA", "NOVA SCOTIA", "NEWFOUNDLAND", "SASKATCHEWAN", "MANITOBA", "NEW BRUNSWICK", "PRINCE EDWARD ISLAND")
    apply(sold,c(1,2), mean)
    print(sold)
}

## Test case

set.seed(10)
mat1 = matrix(r, nrow=20, ncol=20)
prodSold(mat1)










    
    
    
    



