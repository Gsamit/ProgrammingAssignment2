## makeCacheMatrix takes the original matrix and creates some functions that get called by
## cacheSolve. cacheSolve tries to retrieve the data from the solve(matrix) function. If it is there
## it prints it, if not, it calculates it, stores it, and prints it.

makeCacheMatrix <- function( m = matrix() ) {

    i <- NULL					## Initialize the inverse property

    set <- function( matrix ) {			## Method to set the matrix
            m <<- matrix
            i <<- NULL
    }

    get <- function() {				## Method the get the matrix
    	## Return the matrix
    	m
    }

    setInverse <- function(inverse) {		## Method to set the inverse of the matrix
        i <<- inverse
    }

						## Method to get the inverse of the matrix    
    getInverse <- function() {			
        ## Return the inverse property
        i
    }

    						## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve should get passed the value set from makeCacheMatrix. It
## will check the value of "s" from the above code and if it is empty, 
## calculate solve(matrix), return (and store) the value

cacheSolve <- function(x, ...) {

    						## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    						## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    						## Get the matrix from our object
    data <- x$get()

    						## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    						## Set the inverse to the object
    x$setInverse(m)

    						## Return the matrix
    m
}
