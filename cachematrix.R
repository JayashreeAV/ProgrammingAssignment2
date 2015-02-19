## The makeCacheMatrix() function uses 4 functions:
## set_mat: Sets the value of the matrix
## get_mat: Gets the value of the matrix
## set_inv: Sets the inverse of the above matrix
## get_inv: Gets the inverse the above matrix

makeCacheMatrix <- function(x = matrix())
{
    mat_inv <- NULL              ## Erases any old values and sets the mat_inv to NULL
    set_mat <- function(y)       ## This function accepts the user entered matrix values
                                 ##    and assigns it to x
    {
        x <<- y
        mat_inv <<- NULL         ## Sets the mat_inv to NULL
    }
    get_mat <- function() x      ## This function will return the accepted matrix
    set_inv <- function(new_inv) mat_inv <<- new_inv  ## This function will set the
                                                      ##  inverse of the above matrix
                                                      ##  calculated in the cacheSolve()

    get_inv <- function() mat_inv               ## This function will give the inverse
                                                ##    of the matrix
    list(set_mat = set_mat, get_mat = get_mat,  ## this list is used by the makeCachematrix()
        set_inv = set_inv, get_inv = get_inv)   ##   to interact with the cacheSolve().

}

## The below function is used to calculate the inverse of the matrix by checking
##   the cache.
cacheSolve <- function(x, ...) {
    mat_inv <- x$get_inv()   ## Get the inverse of the matrix and store it in
                             ##    mat_inv
    if(!is.null(mat_inv))    ## If the mat_inv value is not NULL, then return the
  {                           ##   mat_inv and display the message
        message("getting cached data")
        return(mat_inv)
    }

    data <- x$get_mat()         ## If mat_inv is NULL, put the matrix values into
                                ## data.
    mat_inv <- solve(data,...)  ## Calculate the inverse of the matrix
    x$set_inv(mat_inv)          ## Set the inverse value of the matrix
    mat_inv                     ## return the new value
}
