## Function description ##
# This function creates objects that can cache the input object (a matrix) and the object's inverse. 
# The function checks if the inverse has already been calculated for the matrix:
#   * If this is the case the inverse matrix is returned. 
#   * If this is not the case the inverse matrix is calculated and cached.

## What the functions do ##
# makeCacheMatrix function:   Makes a list with the four functions set, get, setinversematrix, getinversematrix and 
#                             the object x and m in cache.
# cacheSolve function:        Uses the function makeCacheMatrix to return the inversematrix of object x.

#makeCacheMatrix function
# INPUT: Input argument for makeCacheMatrix is a matrix, object x. The default for object x is an empty matrix. 
# 1.  In the function the object m gets value null. 
# 2.  The set function gets the value of x from the makeCacheMatrix argument and assigns this to object x in the parent environment. 
#     Object m is set to the value of zero in the parent environment. 
# 3. The get function gets the value of object x.
# 4. The setinversematrix function sets the inversematrix object in object m in the parent environment
# 5. The getinversematrix function gets object m that contains the inversematrix object.
# OUTPUT: A list of the functions set, get, setinversematrix, getinversematrix is returned and the objects x and m.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x # gets the matrix
  setinversematrix <- function(inversematrix) m <<- inversematrix
  getinversematrix <- function() m
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


# cacheSolve function
# INPUT: Input argument is the object containing the list created by makeCacheMatrix.
# 1. First calls the object getinversematrix to extract the object m. Object m has as a value either Null
#     or the previously calculated inverse matrix for object x.
# 2. If the value of object m is NOT equal to zero, the value of object m is the inverse matrix of object x. Therefore, object m is returned.
# 3. If the value of object m is equal to zero, the value of object x is extracted from the 'get' object* and assigned as value to object m.
#   (*the 'get'object is an element from the makeCacheMatrix list that was the input object for cacheSolve).
# 4. Then m is included as input in the setinversematrix function object to set the inverse matrix.
# OUTPUT: Object m with as the value the inverse matrix of object x. 

cacheSolve <- function(x, ...) {
  m <- x$getinversematrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversematrix(m)
  m
}
