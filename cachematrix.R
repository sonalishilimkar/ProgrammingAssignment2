## Name : Sonali A. Shilimkar
## makeCacheMatrix function: Code to set matrix, get matrix, set value of inverse, get value of inverse

makeCacheMatrix <- function(x = matrix()) {
# assign null matrix
        inv <- NULL

# set a matrix
        set <- function(y) {

# <<- operator which can be used to assign a value to an object in an environment 
# that is different from the current environment.
                x <<- y
                inv <<- NULL
        }

# get matrix
        get <- function() x

# set inverse of matrix
        setinv <- function(inverse) inv <<- inverse

# get inverse matrix
        getinv <- function() inv
		
		# return a list with the get, set, setinv and getinv functions
        list(set = set, get = get,setinv = setinv,getinv = getinv)
}

## cacheSolve function: Calculates inverse of the matrix. First checks if inverse has is already
## calculated. If calculated, then gets inverse matrix from the cache. Otherwise calculates
## inverse using solve method and sets the inverse matrix in the cache using setinv function
cacheSolve <- function(x, ...) {

		# get inverse matrix
        inv <- x$getinv()
		
		#if already inverse of matrix is calculated, then return the cached inverse matrix
        if(!is.null(inv)) {
                message("Cached Inverse Matrix")
                return(inv)
        }
		
		# else get the matrix and calculate the inverse using solve function
		# this will usually happen the first time.
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}



x <- rbind(c(2, 1), c(1, 2))
m <- makeCacheMatrix(x)
m$get()
cacheSolve(m)
cacheSolve(m)


