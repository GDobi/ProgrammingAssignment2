## creates functions for a special matrix object
## used as the input for cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  m.inverse=NULL
  set=function (y) {
    x<<-y
    m.inverse<<-NULL
}
  get=function()x
  set.inverse = function(inverse) m.inverse <<- inverse
  get.inverse = function() m.inverse
  list(set=set, get=get, set.inverse=set.inverse, get.inverse=get.inverse)
}


## returns the inverse of the original matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m.inverse = x$get.inverse
        ##checks if the inverse has already been calculated
  if (!in.null(m.inverse)){
    return(m.inverse)
  }
        ##if not, then calculates the inverse
  mat.data = x$get()
  m.inverse = solve(mat.data, ...)
  x$setinv(m.inverse)
  return(m.inverse)
}
