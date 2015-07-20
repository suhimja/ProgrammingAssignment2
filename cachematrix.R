## Give following in the console (R or RStudio)
# 1. mat<-makeCacheMatrix()
# 2. mat$set(matrix(1:4,2,2))
# 3. cacheSolve(mat)

## OR to make it more interessting, try the test function.
# 1. define mat as a matrix
# 2. test(mat)


# i have changes the names f set and getmeans to set and getmat
# the setmat sets the result of solve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(solve) m <<- solve
  getmat <- function() m
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmat() # gets matrix from upper function
  if(!is.null(m)) { # if there is a value, means not null
    message("getting cached data") # throw this messsage before result
    return(m)
  }
  data <- x$get() # if not get the matrix
  m <- solve(data, ...) # apply solve(data)
  x$setmat(m) # call set matrix on the upper function and store m in it
  m # give m
}

# test function, so u just have to provide a square invertible matrix, and it runs
# the above defined functions
test = function(mat){
  # mat should be a square invertible matrix
  
  temp_use = makeCacheMatrix(mat) # call make functions
  for (i in 1:3){ # take 3 measurements
    start = Sys.time() 
    cacheSolve(temp_use) # will be done 3 times
    duration = Sys.time() - start
    print(duration) # the duration of each run will be schon
  }
}
