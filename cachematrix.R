## Put comments here that give an overall description of what your
## functions do

## Stores the matrix as a global variable (cached)

makeCacheMatrix <- function(x = matrix()) {
	cachedInversedMatrix <<- x
}


## Returns an uncached matrix for each new matrix used in the function call
## otherwise returns a cached matrix

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'

	if (exists("cachedInversedMatrix")) {
		## we may have already solved the inverse but 
		## is it for the same original matrix? - check it

		inverseOfCached <- solve(cachedInversedMatrix)
		if (isTRUE(all.equal(inverseOfCached,x)) )
			s <- cachedInversedMatrix ## from cache
		else {
			s <- solve(x)
			makeCacheMatrix(s)  ## store in cache 
			}
	} else { ## first call to function
		s <- solve(x)
		makeCacheMatrix(s)   ## store in cache
				
		}
	return(s)

}
