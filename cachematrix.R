## This pair of functions serves to store a matrix and return the inverse or 
## cached invers if already computed

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## x is the input matrix
    
    inv <- NULL ## inv will store the inverse of the matrix and is reset
                ## to NULL every time makeCacheMatrix is called
    
    set <- function(y) { ## takes an input matrix
        
        x <<- y ## saves the input matrix
        
        inv <<- NULL ## resets the inverse to NULL
        
    }

    get <- function() {x} ## this function returns the value of the
                          ## original matrix
    
    setinv <- function(inverse) { inv <<- inverse} 
        ## this is called by cacheSolve() during the first cacheSolve() access
        ## and it will store the inverse using superassignment
    
    getinv <- function() {inv} ## this will return the inverse to cacheSolve()
                               ## on subsequent accesses
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
        ## creates a list containing the internal functions
    
    
}



## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves the 
## inverse from the cache

cacheSolve <- function(x, ...) { ## the input x is an object created by makeCacheMatrix
    
    inv <- x$getinv() ## accesses the object 'x' and gets the value of the inverse
        
    if(!is.null(inv)) {  ##check to see if inverse was already cached (not NULL)
        
        message("getting cached data")  ## message to consol
        
        return(inv) ## return the inverse
        
    }
    
    data <- x$get() ## if inverse not cached load object 'x' 
    
    inv <- solve(data) ## use function solve() to calculate the inverse of the matrix
    
    x$setinv(inv) ## store the value of the inverse of the matrix 'x'
    
    return(inv) ## returns the inverse
    
}