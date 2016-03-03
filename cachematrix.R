
## Below are a pair of functions that are used to create a special object that stores a matrix and caches its inverse.

## The first function, 'makeCacheMatrix' creates a special matrix object that can cache its inverse.

## 'makeCacheMatrix' returns a list containing functions to:
	  ## 1. set the value of the matrix
	  ## 2. get the value of the matrix
	  ## 3. set the value of the inverse
      ## 4. get the value of the inverse
      
makeCacheMatrix <- function(x = matrix()) {            
	 
	## inv will store the cached inverse matrix    
    inv <- NULL   
       
    ## Setter for the matrix     
    ## use '<<-' to assign a value to an object in an environment different from the current environment. 
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Getter for the matrix
    get <- function() x
    
    ## Setter for the inverse
    setInverse <- function(inverse) inv <<- inverse
    
    ## Getter for the inverse
    getInverse <- function() inv
    
    ## return the matrix with the newly defined function
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The second function "cacheSolve" computes the inverse of the speicla "matrix" created by makeCacheMatrix above. If the inverse has alrady been calculated from the cache, it returns the cached inverse.

## 'cacheSolve' returns a matrix that is the inverse of 'x'
## @x: output of the makeCacheMatrix()
cacheSolve <- function(x, ...) {
   
    inv <- x$getInverse()
    
    ## If the inverse has already been calculated, returns the cached inverse withouting computing it repeatedly.
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## Otherwise, calculates the inverse
    mat <- x$get()
    inv <- solve(mat, ...)
    
    ## Cache the value of the inverse
    x$setInverse(inv)
    
    ## Return it
    inv
}