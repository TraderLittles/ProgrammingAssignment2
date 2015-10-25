## These functions calculate and cache the inverse of a matrix in order to preserve processing time

## Testing logic:
## x <- matrix(c(2, 3, 5, 8, 3, 9, 2, 6, 4, 1, 4, 6, 3, 9, 0, 2), c(4, 4))
## xMatrix <- makeCacheMatrix(x)
## cacheSolve(xMatrix)

## this function creates a list of functions that get and set the value of the matrix and its inverse
makeCacheMatrix <- function(xMatrix = matrix()) {
	matrixInverse <- NULL
	fSet <- function(yMatrix){
		xMatrix <<- yMatrix
		matrixInverse <<- NULL
	}
	fGet <- function(){
		xMatrix
	}
	fSetInverse <- function(solve){
		matrixInverse <<- solve
	}
	fGetInverse <- function(){
		matrixInverse
	}
		list(
			fSet = fSet, 
			fGet = fGet,
			fSetInverse = fSetInverse,
			fGetInverse = fGetInverse
		)
}

## this function checks to see if the inverse of the given matrix has been calculated 
## if the function determines that the inverse was calculated aka cached, it returns the cached inverse
## if the function determines that the inverse was not cached, then it calculates and returns the inverse
cacheSolve <- function(xMatrix, ...) {
        xMatrix_local <- xMatrix$fGetInverse()
		if(!is.null(xMatrix_local)){
			 message("getting cached data")
			 return(xMatrix_local)
		}
		matrixData <- xMatrix$fGet()
		xMatrix_local <- solve(matrixData,...)
		xMatrix$fSetInverse(xMatrix_local)
		xMatrix_local
}

