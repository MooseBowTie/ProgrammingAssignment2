## These functions allow the user to store and retrieve information about a matrix.



## This function stores information about a matrix, and returns a list of four functions.
## The function allow users to get and set the values of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) 
{	# Initialize the value of the inverse to null
	inverse <- NULL
	
	# Create function to get and set the matrix and its inverse
	setMatrix <- function(newMatrix)
	{	x <<- newMatrix
		inverse <<- NULL
	}

	getMatrix <- function()
	{	x
	}

	setMatrixInverse <- function(newInverse)
	{	inverse <<- newInverse
	}

	getMatrixInverse <- function()
	{	inverse
	}

	# Return a list of functions
	list(set=setMatrix, get=getMatrix, setInverse=setMatrixInverse, getInverse=getMatrixInverse)
}



## This function returns the cached value of a matrix inverse, if it is available.
## If no cached value is found, the inverse is calculated, cached, and then returned.

cacheSolve <- function(x, ...) 
{	# Retrieve the matrix inverse, if it is available
	inverse <- x$getInverse()

	# If the value of the inverse is not null, return it
	if(!is.null(inverse))
	{	message("Getting cached matrix inverse")
		return(inverse)
	}

	# If the stored value of the inverse was null, then calculate the inverse, store it, and return it.	
	someMatrix <- x$get()
	inverse <- solve(someMatrix, ...)
	x$setInverse(inverse)
	inverse
}
