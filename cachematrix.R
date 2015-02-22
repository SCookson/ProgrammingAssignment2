## Two functions to cache the inverse of a matrix
## For brevity, we are assuming that the matrix can be inverted

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

## makeCacheMatrix generates a list of 4 newly created functions:
## i.   set: which recreates the original matrix
## ii.  get: which reaches back into the calling environment to retrieve the above matrix
## iii. setinverse: generates the inverse of the above matrix and caches it
## iv.  getinverse: reaches back into the calling environment to retrieve the inverted matrix

makeCacheMatrix <- function(x = matrix()) {	
      m <- NULL  		## 'm' is an empty matrix within the makeCacheMatrix environment
      set <- function(y) {	## i. Function recreating original matrix
      x <<- y  		  ## Deep assignment: so inputted matrix 'x' can be recalled outside of this environment 
      m <<- NULL		## Deep assignment: reset 'm' to be an empty matrix outside this environment
                    ## So later on we don't call the wrong matrix from the cache - could happen if we run these functions on multiple matrices
      }
      get <- function() x	        ## ii. Function returning matrix 'x'
      setinverse <- function(solve) m <<- solve	## iii. Function calculating the inverted matrix
      getinverse <- function() m  ## iv. Function returning the inverted matrix
      list(set = set, get = get,  ## list the four functions created
      setinverse = setinverse,
      getinverse = getinverse)
}


## cacheSolve: Returns a matrix that is the inverse of the matrix 'x'
cacheSolve <- function(x, ...) {
      m <- x$getinverse()   ## define matrix 'm' to be the cached inverted matrix 
      if(!is.null(m)) {     ## check for the inverted matrix in the cache
      ## if it's a cache hit ...
      ## (because we have already run this function once & haven't changed matrix)
      ## this code will run quickly
      message("getting cached data")  ## let the user know we are retrieving from the cache
      return(m)             ## return the inverted matrix from the cache
      }                           
      ## if it's a cache miss ...
      ## (because it's the 1st time we have run this function on this matrix OR we changed matrices at some point)
      ## this code may take a some time to run if the matrix is large
      data <- x$get()       ## get function retrieves the original matrix 'x'
      m <- solve(data, ...) ## invert the retrieved matrix
      x$setinverse(m)       ## cache the inverted matrix for future use
      m                     ## return the inverted matrix
}
