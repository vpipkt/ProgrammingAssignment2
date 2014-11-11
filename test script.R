#test script for matrix functions

jj<- makeCacheMatrix(matrix(1:4,nrow=2))
rm(bb)

bb<-makeCacheMatrix(matrix(c(4,0,-1,2),nrow=2))

bb$get()
bb$getInverse()

cacheSolve(bb)
cacheSolve(jj)

jj$getInverse()

bb$getInverse() %*% bb$get()

jj$get() %*% jj$getInverse() == diag(nrow(jj$get()))

