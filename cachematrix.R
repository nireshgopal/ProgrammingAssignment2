## R program to define funcations makeCacheMatrix and cacheSolve
## These functions would be used to inverse a matrix and cache the result for 
## future usage

## makeCacheMatrix() function creates a "special" vector containing the list
## of functions

makeCacheMatrix <- function(x = matrix()) {
        
        ##  set an dummy variable for caching the inverse 
        m <- NULL
        
        ## Define funcation set() to set the input matrix to x
        ## and also clears the cache 
        set <- function(y) {          
                x <<- y
                m <<- NULL
        }
        
        ## Define function get() to return the input matrix
        get <- function() x
        
        ## Define function setinverse() to set the inverse of input matrix to m
        setinverse <- function(inv) m <<- inv
        
        ## Define funcation getinverse() to return the inverse of input matrix 
        getinverse <- function() m
        
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve() function derives the inverse of a matrix using Solve function 
## Function will look into the cache if the inverse is already calcuated 
## If not, the inverse of the given matrix is calcuated 
## and stored for future reference. 

cacheSolve <- function(x, ...) {  
        
        ## call the getinverse() to see if the inverse is cached already 
        m <- x$getinverse()
        
        ## if inverse is cached return otherwise go ahead and calcualte the inverse
        if(!is.null(m)) {
                message("Getting inverse from cache")
                return(m)
        }
        
        ## call the get() to fetch the input matrix 
        mtrx <- x$get()
        
        ## call solve() function to inverse the inpput matrix
        m <- solve(mtrx)
        
        ## call the setinverse function to set the inverse to cache
        x$setinverse(m)
        
        m
        
}
