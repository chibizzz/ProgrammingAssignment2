## The code below is designed to compute the inverse to the given matrix (if it is invertible) implementing the possibility to store
## results of the computation in cache. Hence, if the function is presented with the same matrix (the matrix has not been changed
## from the previous step), it retrieves the inverse from cache. Otherwise, the inverse is computed from scratch. This approach
## allows to economize on time needed for execution (since the computation of the inverse can be time-consuming).

## This function creates a list of 4 functions which allow you to set 
## the target matrix, get the value of that matrix, set the inverse of the matrix and get that matrix, in that order.
## This function also contains the inverse of the target matrix in its environment, making it resemble the cache.

makeCacheMatrix <- function(x = matrix()) {
	inverse<-NULL
	set<-function(y) {
		x<<-y
		inverse<<-NULL
	}
	get<-function () x
	setinverse<-function(solve) inverse<<-solve
	getinverse<-function () inverse
	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function either takes the inverse of the given matrix from cache (that is, from the environment of the first function),
## if it has already been computed, or computes the inverse and then saves it to the cache, so that you do not have to recompute it once again.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}





















