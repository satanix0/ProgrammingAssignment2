## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # initialize the inverse variable
        inv <- NULL
        # Function to set the matrix value and invalidate the inverse
        set <- function(mat) {
                x <<- mat
                inv <<- NULL
        }

        # function to set the matrix value
        get <- function() {
                x
        }

        # function to set the inverse of the matrix
        setInverse <- function(inverse) {
                inv <<- inverse
        }

        getInverse <- function() {
                inv
        }

        #  return a list of the functions
        list(
                set = set, get = get,
                setInverse = setInverse, getInverse = getInverse
        )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        #  get the matrix from the cache
        matrix <- x$getInverse()

        #  check if the inverse is already cached
        if (!is.null(matrix)) {
                message("Returning Cached Inverse")
                return(matrix)
        }

        # Get the matrix from our project
        val <- x$get()

        # Compute the inverse using matrix multiplication
        matrix <- solve(val) %*% val

        # Cache the inverse
        message("Cached Inverse")
        x$setInverse(matrix)

        # return the inverse
        matrix
}
