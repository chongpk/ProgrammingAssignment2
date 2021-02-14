

## a special matrix object created for caching its inverse
makeCacheMatrix <- function( mat = matrix() ) {

	## The inverse property is initialized
    inv <- NULL

    ## Function to set the matrix
    set <- function( matrix ) {
            mat <<- matrix
            inv <<- NULL
    }

    ## Function the get the matrix
    get <- function() {
    	## Return the matrix
    	mat
    }

    ## Function to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Functionto get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        inv
    }

    ## a list of the methods to be returned
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix" above.

cacheSolve <- function(x, ...) {

    ## a matri with inverse of 'x' was returned
    mat <- x$getInverse()

    ## return the inverse which  its already set
    if( !is.null(mat) ) {
            message("getting cached matrix")
            return(mat)
    }

    ## Get the matrix from our object
    data <- x$get()

    ##  matrix multiplication was used to claculate the inverse
    mat <- solve(data) %*% data

    ##  object was set to inverse
    x$setInverse(mat)

    ## Return the matrix
    mat
}