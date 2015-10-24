## Coursera, R Programming
## Assignment 2
## October 25, 2016

## Lydia Musher
## lmusher@gmail.com

## Create, cache, and return the inverse of an inversible matrix.

## Create and cache the inverse of an inversible matrix.

makeCacheMatrix <- function(x = matrix()) {

	# This is more or less the same as the makeVector example.
	# I added some comments so that it would be easier to read.


	# Create the matrix that will live in the global environment.
        m <- NULL

	# Cache the matrix.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

	# Get the cached matrix.
        get <- function() x

	# Cache the inverse.
        setinverse <- function (inv) m <<- inv

	# Get the inverse of the matrix.
        getinverse <- function () m

        list(set = set, 
		 get = get,
             	 setinverse = setinverse,
             	 getinverse = getinverse)
}

## Return the cached inverse of a matrix.

cacheSolve <- function(x, ...) {

	# First we check the cache.
        m <- x$getinverse()

        if(!is.null(m)) {

		# If it's already cacheed, return it and drop out of the function.
                message("Getting cached data.")
                return(m)

        }

	# If we've gotten to this line, it wasn't already cached,
	# so we have to get the original, process, cache, and return it.
        data <- x$get()

	# This is the money line. It calculates the inverse and puts it into cache.
	# We use the function "solve" as indicated.
	m <- solve(data, ...)
	
	# Now set it into cache.
        x$setinverse(m)

	# Now reeturn m, the cached matrix inverse.
        m

}
