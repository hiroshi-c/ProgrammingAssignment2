## This code handles a special R object which stores a matrix
## and cached inverse matrix corresponding to the matrix.
## makeCacheMatrix funciton creates an instance of the object and
## cacheSolve function updates stored inverse matrix as needed.
## To simplify the assignment logistic,
## we assume that the matrix supplied is always invertible.

## Note 1: I do not like two aspects of coidng sytle of makeVector() example.
##   1. Parentheses are omitted time to time. That causes losing symmetry of the code.
##   2. Variable names are not explanatory.
## I did not follow these, therefore the style is different from the example.
## I hope my style is still understandable for evaluation.

## Note 2: I don't understand why cacheSolve needs to be a separate function.
## I think it's more meaningful to be one of member methods of the cache matrix object,
## or just a part of setInverse function.
## However, I did not go that far because assignment template already gives that method
## and such change would confuse the peer evaluation.

## Construct an instance of special matrix object explained above.
## Optional parameter matrixEntity may specify the matrix itself at construction time.
makeCacheMatrix <- function(matrixEntity = matrix()) {
    # object to hold cached inverse matrix. NULL means not cached yet.
    cachedInverseMatrix <- NULL

    # functions associated with this R object.
    list(
        get = function(){
            matrixEntity
        },
        set = function(newMatrix){
            matrixEntity <<- newMatrix
            cachedInverseMatrix <<- NULL
        },
        getInverse = function(){
            cachedInverseMatrix
        },
        setInverse = function(inverseMatrix){
            cachedInverseMatrix <<- inverseMatrix
        }
    )
}

## Calculate the inverse matrix of special matrix object.
## This does no-op if the inverse matrix is already cached.
## This returns the inverse matrix, regardless cached value is caclculated or not.
cacheSolve <- function(cacheMatrix, ...) {
    result<-cacheMatrix$getInverse()
    if(is.null(result))
    {
        message("Inverse matrix is being calculated and returned.")
        result<-solve(cacheMatrix$get())
        cacheMatrix$setInverse(result)
    }
    else
    {
        message("Returning cached inverse matrix.")
    }
    result
}


## Test sequence

## Test matrix 1 - set by constructor
X1<-matrix(c(2,4,3,1,5,7,7,4,2), nrow=3, ncol=3)
CacheX<-makeCacheMatrix(X1)
print('--- matrix X1')
print(X1)
print('--- inverse matrix of X1')
print(solve(X1))
print('--- first call - should see the diagnosis message \'Inverse matrix is being calculated and returned.\'')
print(cacheSolve(CacheX)) #
print('--- second call - should see the diagnosis message \'Returning cached inverse matrix.\'')
print(cacheSolve(CacheX))

## Test matrix 2 - setter
X2<-matrix(c(5,8,9,-4,0,-4,-7,3,-5,2,7,-1,4,0,6,-8), nrow=4, ncol=4)
CacheX$set(X2)
print('--- matrix X2')
print(X2)
print('--- inverse matrix of X2')
print(solve(X2))
print('--- first call - should see the diagnosis message \'Inverse matrix is being calculated and returned.\'')
print(cacheSolve(CacheX)) #
print('--- second call - should see the diagnosis message \'Returning cached inverse matrix.\'')
print(cacheSolve(CacheX))

