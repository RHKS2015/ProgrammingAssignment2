## Overall description of what the below 2 functions ("makeCacheMatrix"’ and ‘"cacheSolve"’) 
## do is that they are a pair of functions that cache and computes the inverse of a matrix.


## The first function called "‘makeCacheMatrix"’ creates a special ‘matrix’ object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    
		set <- function(y) {
			x <<- y
			i <<- NULL
			}
		
    get <- function() x
		setinverse <- function(inv) i <<- inv
		getinverse <- function() i
		
		list ( set = set, get = get, 
			setinverse = setinverse, getinverse = getinverse)
}


## The second function below called "‘cacheSolve"’ calculates the inverse of the special ‘matrix’ 
## returned by the "‘makeCacheMatrix"’ function created above.  Here, if the inverse has been 
## already calculated (and matrix has not been changed), then "‘cacheSolve"’ function retrieves 
## the inverse from the cache.  

cacheSolve <- function(x, ...) {
  
		i <- x$getinverse()

		if(!is.null(i)) {
			message("Getting Cached Data")
			return(i)
		}

		data <- x$get()
		i <- solve(data, ...)
		x$setinverse(i)
    
    ## Return a matrix that is the inverse of 'x'
		i
}
