## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.
## The following two functions are used to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Set the inverse equal to NULL
        MInv <- NULL
        set <- function (y) {
                x <<- y
                ## Re-set Inverse Matrix 'MINv' to NULL, this is important if matrix 'x' is redefined.
                MInv <<- NULL
        }
        ## get function returns the matrix
        get <- function()x
        setinverse <- function (Inverse) MInv <<- Inverse
        # getInverse returns the Inverse
        getinverse <- function() MInv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## This function assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        Inv <- x$getinverse()
        ## If the Inverse of Matrix has already been calculated, cacheSolve returns that value
        if(!is.null(Inv)){
                message("Getting cached data.")
                return(Inv)
        }
        ## If the inverse of the Matrix has not been calculated (NULL) ,then to get the Matrix 'x' the get function is called, 
        ## The inverse of the Matrix 'Inv' is calculate, and sets the value in the cache via setinverse function.
        data <- x$get()
        Inv <- solve(data)
        x$setinverse(Inv)
        ## Return a matrix(Inv) that is the inverse of 'x'
        Inv
}

## How to Use
## Set the row size of the Matrix
## r <- 5
## Set the column size of the Matrix (Square Matrix)
## c <- r 
## Create a matrix
## x <- matrix(sample(1:50, r*c, replace=T),r,c)
## Call makeCacheMatrix function
## m<-makeCacheMatrix(x)
## cacheSolve(m) ## No cache in the first run, the function return the inverse of Matrix
## cacheSolve(m) ## Return the inverse of Matrix **From cached data
## Create a New Matrix to validade the cached inverse Matrix
## x <- matrix(sample(1:100, r*c, replace=T),r,c)
## m$set(x)
## m$get()
## cacheSolve(m) ## No cache in the first run, the function return the inverse of Matrix
## cacheSolve(m) ## Return the inverse of Matrix **From cached data

