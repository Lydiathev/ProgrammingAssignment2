## The first function, makeCacheMatrix, contains a special "matrix", which is really a list containing a function to 
## 1. set up a matrix
## 2. get the value of a matrix
## 3. set the inverse of a matrix
## 4 get the value of the inverse of a matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        Inverse <- NULL
        set <- function(y) {
                x <<- y
                Inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) Inverse <<- Inverse
        getInverse <- function() Inverse
        list(set = set, get = get,
             setInverse = Inverse,
             getInverse = Inverse)
}

## The following function calculates the inverse of a special "matrix" created with the above function. It checks if the inverse has already been calculated and if so, gets the inverse from the cache and skips to the computation. Otherwise it calculates the inverse of the matrix and sets the inverse matris in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inverse <- x$getInverse()
        if(!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        data <- x$get()
        Inverse <- solve(data, ...)
        x$setInverse(Inverse)
        Inverse
}
}
