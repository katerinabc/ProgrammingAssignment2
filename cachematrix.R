# makeCatchMatrix creates a special "matrix" object
#that can cache its inverse.

# cacheSolve computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){# code for the set function
        x <<- y #assigns the value y to x in the environment of function(y). the original object x is not changed
        m <<- NULL #assignes NULL to the object m
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve #creates the object setinverse. Setinverse is a function called solve. this function is defined as 'solve' which calculates the inverse of a matrix. solve is defined outside the environment of function(x=matrix()) as it is a basic R function
    getinverse <- function() m #returns the inverse if it has been already calculated
    list(set = set, get = get, #creates a list with the different functions using the function names as names for the objects in the list
    setinverse = setinverse,
    getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    m <- x$getinverse() #This assignes the value of getinverse of x to m
    if(!is.null(m)) {#this checks if the inverse of m has already been calculated. if yes it prints the message ("getting cached data" and returns m which has been calculated in the function makeCacheMatrix
            message("getting cached data")
            return(m) # m is returned if it has been calculated previously
        }
    data <- x$get() #this assignes x to data to compute the inverse (in the next line)
    m <- solve(data, ...) #calculates the inverse of data and assignes it to m
    x$setinverse(m)
    m #returns m
}
