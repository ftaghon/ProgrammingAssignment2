## Special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    Ix = NULL
    set <- function(y) {
        x <<- y
        Ix <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) Ix <<- solve
    getinverse <- function() Ix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function that computes and cache the inverse of special matrix created with makeCacheMatrix
cacheSolve <- function(x, ...) {
    Ix <- x$getinverse()
    if(!is.null(Ix)) {
        message("getting cached data")
        return(Ix)
    }
    data <- x$get()
    Ix <- solve(data, ...)
    x$setinverse(Ix)
    Ix
}

### First test with the identity matrix ----
# Creation of the identity matrix
id_mat = matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
print(id_mat)

# Classical computation ofthe inverse
# The inverse of the identity matrix is the identity matrix
solve(id_mat)

# Creating the special identity matrix
id_cacheMat = makeCacheMatrix(id_mat)

# Checking that the default inverse is NULL
id_cacheMat$getinverse()

# Computing and caching the inverse of the special identity matrix
cacheSolve(id_cacheMat)

# Checking the cached inverse matrix
id_cacheMat$getinverse()


### Second test with a 90° rotation matrix
# Creation of the 90° rotation matrix
rot_mat = matrix(c(0, -1, 1, 0), nrow = 2, ncol = 2)
print(rot_mat)

# Classical computation ofthe inverse
# The inverse of the 90° rotation matrix is the -90° rotation matrix
solve(rot_mat)

# Creating the special 90° rotation matrix
rot_cacheMat = makeCacheMatrix(rot_mat)

# Checking that the default inverse is NULL
rot_cacheMat$getinverse()

# Computing and caching the inverse of the special rotation matrix
cacheSolve(rot_cacheMat)

# Checking the cached inverse matrix
rot_cacheMat$getinverse()


