## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat_x = matrix()) {
    inversed <- NULL # Default value as Null
    new_mat <- function(input) { # Function to set the new value of a matrix
        mat_x <<- input 
        inversed <<- NULL
    }
    get_mat <- function() mat_x # Fetch the matrix value 
    inv_mat <- function(inv) inversed <<- inv #calling function to inverse a matrix
    get_inv <- function() inversed # Function to get the inversed matrix
    return(list(new_mat = new_mat, get_mat = get_mat, inv_mat = inv_mat, get_inv = get_inv))
}


## Write a short comment describing this function

cacheSolve <- function(mat_x, ...) {
    inversed <- mat_x$get_inv() # Store the inverse of the input mat_x
    if(!is.null(inversed)) {
        message("Getting last processed data...") # User readable message to hightlight fetching from cache
        return(inversed)
    }
    data <- mat_x$get()
    invsersed <- solve(data, ...)
    mat_x$inv_mat(inversed)
    return(inversed)
}
