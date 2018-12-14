## Two functions for caching inverse of matrix
## makeCacheMatrix build an object class containing the matrix and calculated inverse and methods to access them.
## cacheSolve determines whether the makeCacheMatrix object have an inversed calculated. If yes, then return the
## inverse. If not, calculate the inverse and update the makeCacheMatrix object



## makeCacheMatrix accepts a matrix as an argument. It contains variable inverseMatrix, which is the cached inverseMatrix.
## inverseMatrix is initialized as null. 
## The object has 4 functions 
## 1. set() allows the object to be set with a new matrix
## 2. get() gets the matrix back
## 3. setInverse() sets the inverseMatrix of the object. inverseMatrix is initialized as null.
## 4. getInverse() gets the inverseMatrix of the object. 

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseMatrix_val) inverseMatrix <<- inverseMatrix_val
    getInverse <- function() inverseMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve takes a makeCacheMatrix object x.
## If the x$getInverse() is null, cacheSolve calculates the inverse matrix and updates the object.
## Else, it returns the cached inverse matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix)) {
        message("getting inverted matrix")
        return(inverseMatrix)
    } else{
        matrix <- x$get()
        inverseMatrix <- solve(matrix)
        x$setInverse(inverseMatrix)
        inverseMatrix
    }
}

## test code: 
## initialize invertable matrix a

#a <- matrix(c(-1, 1.5, 1, -1), nrow = 2, ncol = 2 )

## initialize makeCacheMatrix b from a. test b functions

#b <- makeCacheMatrix(a)
#b$get()
#b$getInverse()
#cacheSolve(b)
#b$getInverse()
#b$set(2*a)
#b$get()
#b$getInverse()
#cacheSolve(b)
#b$getInverse()
