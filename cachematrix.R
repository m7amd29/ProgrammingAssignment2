## The two functions defiened here enable you to inverse an invertible matrix.


## The "makeCacheMatrix" function creates a special matrix object the stores the
## cached output of inversing a matrix inside the object itself to be used when
## recomputing the operation again.

makeCacheMatrix <- function(x = matrix()) {
  solved_matrix <- NULL
  set <- function(y) {
    x <<- y
    solved_matrix <<- NULL
  }
  get <- function() x
  set_solved_matrix <- function(the_solve) solved_matrix <<- the_solve
  get_solved_matrix <- function() solved_matrix
  list(set = set, get = get, set_solved_matrix = set_solved_matrix,
       get_solved_matrix = get_solved_matrix)

}


## The "cacheSolve" function makes the inversing computations after checking 
## cached data inside the object itself

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  solved_matrix <- x$get_solved_matrix()
  if(!is.null(solved_matrix)) {
    message("Getting cached data")
    return(solved_matrix)
  }
  matrix_data <- x$get()
  solved_matrix <- solve(matrix_data, ...)
  x$set_solved_matrix(solved_matrix)
  solved_matrix
}
