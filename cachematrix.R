## Two functions to cache the inverse of a matrix
## For brevity, we are assuming that the matrix can be inverted

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 


## The 1st function, makeCacheMatrix, generates a list of 4 newly created functions:
## i.   set: which recreates the original matrix
## ii.  get: which reaches back into the calling environment to retrieve the above matrix
## iii. setinverse: generates the inverse of the above matrix and 
## iv.  getinverse: reaches back into the calling environment to retrieve the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {	
  	  m <- NULL  		## 'm' is an empty matrix within the makeCacheMatrix environment
	    set <- function(y) {	## i. Function recreating original matrix
		  x <<- y  		  ## Deep assignment: so inputted matrix 'x' can be recalled outside of the makeCacheMatrix environment 
		  m <<- NULL		## Deep assignment: so 'm' is still an empty vector outside the makeCacheMatrix environment
      }
	    
	    get <- function() x	        ## ii. Function returning matrix 'x'
	    setinverse <- function(solve) m <<- solve	## iii. Function that calculates the inverse of a matrix within the makeCacheMatrix
	    getinverse <- function() m  ## iv. Function returning the inverted matrix
	    list(set = set, get = get,  ## returns the list of the four functions created in the vector
		  setinverse = setinverse,
		  getinverse = getinverse)
}


## cacheSolve: Returns a matrix that is the inverse of the matrix 'x'
cacheSolve <- function(x, ...) {
    	m <- x$getinverse()   ## define matrix 'm' to be the cached inverted matrix 
	    if(!is.null(m)) {     ## check for the inverted matrix in the cache
      ## cache hit
	    message("getting cached data")
	    return(m)             ## return the inverted matrix from the cache
	    }                     
      ## if it's a cache miss
      data <- x$get()       ## get function retrieves the original matrix 'x'
	    m <- solve(data, ...) ## invert the retrieved matrix
	    x$setinverse(m)       ## cache the inverted matrix for future use
      m                     ## return the inverted matrix
}

