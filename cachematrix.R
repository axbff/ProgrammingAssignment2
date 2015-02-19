## Functions to calculate & cache inverse matrix

## Create data structure that includes original matrix,
## cached inverse matrix, and functions to set/get both

makeCacheMatrix <- function(x = matrix()) {
	xi <- NULL
	
	set <- function(y) {
		x <<- y
		xi <<- NULL
	}

	get <- function() x

	setinv <- function(i) xi <<- i
	getinv <- function() xi

	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Check if inverse matrix is calculated and return cached value,
## otherwise calculate, store in cache, and return result

cacheSolve <- function(x, ...) {
    xi <- x$getinv()

    if(!is.null(xi)){
    	message("getting cached data")
    	return(xi)
    }

    data <- x$get()

    xi <- solve(data, ...)

    x$setinv(xi)

    xi
}