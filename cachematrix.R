##The following pair of functions (makeCacheMatrix and cacheSolve) cache the inverse of a matrix. 

## makeCacheMatrix - creates a special matrix object that can cache its inverse.
##      Input: x - matrix to be cached
## makeCacheMatrix contains the following functions that are used by cacheSolve
##      set - takes as input setThis which will set the cache matrix to its input variable setThis
##      get - will return the cached matrix that was stored using the set function
##      setInverse - will set the value of the inverse 
##      getInverse - will return the value of the inverse
##   

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(setThis) {
               x <<- setThis
               inverseMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inverseMatrix <<- solve
        getInverse <- function() inverseMatrix
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
        
}


## cacheSolve - computes the inverse of the special matrix returned by makeCacheMatrix. 
##      Input x, ... - the list of functions returned by makeCacheMatrix with the, 
##        	       assumes that inputted matrix is invertible
##      Returns the inverse of the inputed matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        theInverseMatrix <- x$getInverse()
        
        if(!is.null(theInverseMatrix)) {
                message("retrieving data that has been cached")
                return (theInverseMatrix)
        }
        
        #there is no cached data, process the inverse of the matrix 
        theData <- x$get()
        theInverseMatrix <- solve(theData)
        x$setInverse(theInverseMatrix, ...)
        return(theInverseMatrix)
}
