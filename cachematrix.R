## R Programming / Week Three / Programming Assignment / Lakhani

### Assignment: Caching the Inverse of a Matrix
 
## The objective of this R program is to compute the inverse of a given matrix and 
## then cache it for reuse.  By caching the "stable" variables in the environment,
## we can avoid unnecessary "recomputations," thus making access to data faster and 
## utilization of computing resources more efficient. 

## The R program comprises into two functions. The first function, makeCacheMatrix(),
## creates a special "matrix" object that can cache its inverse, whereas the second
## function, cacheSolve(), computes the inverse of the special "matrix" returned by the
## makeCacheMatrix segment.  If the inverse has already been calculated (and the matrix 
## has not changed), then the cacheSolve segment just retrieves the inverse from cache.


## 1.  `makeCacheMatrix`: This function creates a special "matrix" object  that can 
## cache its inverse.

## For the purposes of this assignment, we will assume that the matrix supplied is 
## invertible.


makeCacheMatrix <- function(x = matrix()) {
	## x: an invertible matrix
	## return: a list containing functions to
	##	1. set the matrix
	##	2. get the matrix
	##	3. set the inverse
	##	4. get the inverse
	## This list is then used as input to cacheSolve().

	m = NULL
            set <- function(y) {
        	# use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinverse <- function(solve) m <<- inverse
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}

## 2.  `cacheSolve`: This function computes the inverse of the special "matrix" returned 
## by `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix 
## has not changed), then `cacheSolve` retrieves the inverse from the cache.

## Computing the inverse of a square matrix is done with the `solve` function in R. For 
## example, if `X` is a square invertible matrix, then `solve(X)` returns its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
            if(!is.null(m)) {
		# gets it from the cache and skips the computation. 
                    message("getting cached data")
                    return(m)
            }
            
            # otherwise, calculates the inverse
	    data <- x$get()
            m <- solve(data, ...)

        ## sets the value of the inverse in the cache via the setinverse function.
            x$setinverse(m)
            return(m)
}
