## This is an exercise in lexical scoping and environments.  There
## are two functions: makeCacheMatrix and cacheSolve.

        # This function will take a matrix as a parameter.  It contains internal
        # functions that get/set the inverse of the matrix in this environment.
makeCacheMatrix <- function(x = matrix()) {

            # Initialize the inverse matrix as NULL.
    inverse_matrix <- NULL

            # Function to set matrix and initialize the inverse matrix as a NULL.
            # This is needed by cacheSolve to determine if it should perform
            # the matrix inversion (if this is NULL) or return the inverse matrix
            # (if it is not NULL) from this environment.
    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }

            # Function to return the matrix.
    get <- function() x

            # Function to save the inverse matrix in this environemnt.
    set_cache <- function(solve_result) inverse_matrix <<- solve_result

            # Function to return the inverse matrix from this environment.
    get_cache <- function() inverse_matrix

            # The functions are defined in a list
    list( set = set, get = get,
          set_cache = set_cache,
          get_cache = get_cache )
}

        #  Function to return an inverted matrix using the solve function.
        #  The first arg is the matrix, the remaining args are optional values
        #  for the solve() function.  Note that this function is dependent on
        #  the make makeCacheMatrix function.
cacheSolve <- function(x, ...) {

            # Get the inverse matrix from the environment of 'x'.
    inverse_matrix <- x$get_cache()

            # Determine if the inverse has already been processed (i.e. not null), then
            # print a message and return the cached inverse.
    if( !is.null(inverse_matrix)) {
        message("getting cached data")
        return(inverse_matrix)
    }

            # To get to this point in the code, the inverse_matrix must be NULL, so the
            # inverse matrix has not been performed.

            # Get the matrix data.
    orig_matrix <- x$get()

            # Next call solve() to get the inverse.  Note we pass the matrix and then a
            # variable number of optional parameters to the solve() function.
    inverse_matrix <- solve(orig_matrix,...)

            # Save the inverse into cache (i.e. the environment for 'x').
    x$set_cache(inverse_matrix)

            # return the inverse matrix
    inverse_matrix
}

################
## TESTING RESULTS:  I found this exercise a challenge and wanted to make sure it worked.
## I ran the following commands and captured the results for each function.

##create matrix
#     > matrix_data <- matrix( c(1.00, -0.25, -0.25, 1.00), nrow=2, ncol=2)

#     > matrix_data
#            [,1]  [,2]
#       [1,]  1.00 -0.25
#       [2,] -0.25  1.00

##set mytest1
#     > mytest1 <- makeCacheMatrix(matrix_data)

## execute the first time, should not call cache.
#     > cacheSolve( mytest1)
#          [,1]      [,2]
#     [1,] 1.0666667 0.2666667
#     [2,] 0.2666667 1.0666667

## execute second time, should show message "getting cached data"
#     > cacheSolve( mytest1)
#     getting cached data
#          [,1]      [,2]
#     [1,] 1.0666667 0.2666667
#     [2,] 0.2666667 1.0666667

## call $get() to pull original matrix data
#     > mytest1$get()
#           [,1]  [,2]
#     [1,]  1.00 -0.25
#     [2,] -0.25  1.00

## call $get_cache to pul inverse matrix data
#     > mytest1$get_cache()
#          [,1]      [,2]
#     [1,] 1.0666667 0.2666667
#     [2,] 0.2666667 1.0666667

## create a new matrix
#     > matrix_data2 <- matrix( c(-0.25, 1.00, 1.00, -0.25), nrow = 2, ncol=2)

## call $set to reinitialize the data and reset inverse to NULL
#     > mytest1$set(matrix_data2)

## call cacheSolve with the same variable having new matrix data
## and it should not return the previously cached data, rather calculate
## the new inverse.
#     > cacheSolve( mytest1)
#     [,1]      [,2]
#     [1,] 0.2666667 1.0666667
#     [2,] 1.0666667 0.2666667

## call cacheSolve again wit the same variable and it should return
## the newly cached inverse.
#     > cacheSolve( mytest1)
#     getting cached data
#     [,1]      [,2]
#     [1,] 0.2666667 1.0666667
#     [2,] 1.0666667 0.2666667
