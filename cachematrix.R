## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# First let's clean the workspace
# rm(list=ls())

makeCacheMatrix <- function(m = matrix()) { #the function accept as argument a square matrix ixi
    mi <- NULL #first I set the variable m of matrix inversed to Null, because I don't know the inversed matrix
    getm <- function() m #this function return the not inversed matrix
    getmi <- function() mi #this function return the not inversed matrix
    setmi <- function(inversed) mi <<- inversed #this function receive the inversed matrix as argument and use it to set mi. Now mi is not NULL anymore
    list(getm = getm, getmi = getmi, setmi = setmi) 
}
## Write a short comment describing this function

cacheSolve <- function(x, ...){
    mti <- x$getmi() #first I need to know if mi is NULL or if there is a inversed value stored in this variable
    if(!is.null(mti)) { #if there is a stored value, the function only returns it
        return(mti)
    }
    m <- x$getm() #take the original matrix value and assign it to m
    mti <- solve(m) #inverse the matrix m and store it in mti
    x$setmi(mti, ...) #use the first makeCacheMatrix to set it to mi
    # Return a matrix that is the inverse of 'x'
    mti
}


# We need to prepare a matrix to use in the function
# x <- matrix(runif(4**2), 4)
# Let's assign the first function to m, using as argument the matrix x
# m <- makeCacheMatrix(x)
# mi should be null, lets check it
# m$getmi()
# So, as mi is NULL, we need to store the inversed value on it using the second function
# cacheSolve(m)
# Let's check if the inversed function is correctly calculated
# cacheSolve(m) == solve(x)
# Now we can check if mi is armazened in the Cachematrix, i.e, is not NULL
# m$getmi()
# the second function could solve the matrix and store it in the cached function unsing <<- operator
