## The 2 functions below ('makeCacheMatrix' and 'cacheSolve') 
## calculate the inverse of an invertible matrix. The inverse 
## matrix will be cached and retrieved if the matrix does not change. 

## The main funcion 'makeCacheMatrix' contains 4 functions 
## (set, get, setinv, and getinv) that create a matrix object able 
## to cache its inverse. The matrix inverse is stored in 'im'.   

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinv <- function(solve) im <<- solve
        getinv <- function() im
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The funcion 'cacheSolve' searches for the matrix inverse stored in 'im'. 
## If the value of 'im' is NULL, the inverse matrix is calculated, 
## otherwise, the stored value is returned. 

cacheSolve <- function(x, ...) {
        im <- x$getinv()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinv(im)
        im 
}
