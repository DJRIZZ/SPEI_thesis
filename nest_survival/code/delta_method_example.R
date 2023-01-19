###Test out Delta method calculation to make sure I know what I'm doing
###Mark Book, B The 'Delta method' ..., B.4. (B-20)
### Variance of a product of survival probabilities

#list out the survival probabilities.
a <- 0.6109350
b <- 0.458263
c <- 0.4960239

#product of probabilities
productabc <- a*b*c
productabc

###variance covariance matrix of survival estimates
matrix_data <- c(0.0224330125,-0.0003945405,0.0000654469,
                 -0.0003945405,0.0099722201,-0.0002361998,
                 0.0000654469,-0.0002361998,0.0072418858)
matrix_vcv <- matrix(matrix_data, nrow = 3, ncol = 3, byrow = TRUE)
matrix_vcv

###We need to get the partial derivatives for each survival estimate with respect
# to the appropriate parameter
###This will simply be b*c for a and so forth
a_prime <- b*c
b_prime <- a*c
c_prime <- a*b

###make the partial derivatives a list of matrix so you can mulitply
abc_primes <- c(a_prime,b_prime,c_prime)
abc_primes ##the digits of precicion are not exactly same, I'll change it after and do it again

matrix_1 <- matrix(abc_primes, nrow = 1, ncol = 3, byrow = TRUE)
matrix_1

matrix_2 <- matrix(abc_primes, nrow = 3, ncol = 1, byrow = TRUE)
matrix_2

###Lets test it out to get the standard deviation
matrix_1 %*% matrix_vcv %*% matrix_2
#Equals 0.002556409 It matches the book!!




###Another example.
library(RMark)
data(dipper)

# run model
mod=mark(dipper,model.parameters=list(Phi=list(formula=~time)))

# Get estimates of Phi and their v-c matrix
rr=get.real(mod,"Phi",se=TRUE,vcv=TRUE)

# Compute se of the product of the first 3 survivals
deltamethod.special("prod",rr$estimates$estimate[1:3],rr$vcv.real[1:3,1:3]) #0.03488456. Note this is the standard error!

# Compute se of the product of the last 3 survivals
deltamethod.special("prod",rr$estimates$estimate[4:6],rr$vcv.real[4:6,4:6]) #0.03500327

##try square the "se"
0.03488456^2
#equals 0.001216933


###Look at first three estimates
rr$estimates
#0.6258353
#0.4541913
#0.4783772


###look at vcv to get numbers again
rr$vcv.real

###Get the 3 x 3 version of it
vcv_matrix <- as.matrix(rr$vcv.real)
vcv_matrix

###delete columns and rows 4:6 to get first three col and rows of vcv
vcv_3x3_matrix <- vcv_matrix[-c(4:6), -c(4:6)]
vcv_3x3_matrix


###first three survival estimates
a <- 0.6258353
b <- 0.4541913
c <- 0.4783772


###partial derivatives of survival estimastes with respect to a,b, and c (parameters)
a_prime <- b*c
b_prime <- a*c
c_prime <- a*b


###make the partial derivatives a list of matrix so you can mulitply
abc_primes <- c(a_prime,b_prime,c_prime)
abc_primes ##the digits of precicion are not exactly same, I'll change it after and do it again


###Follow the formula from the book
matrix_1 <- matrix(abc_primes, nrow = 1, ncol = 3, byrow = TRUE)
matrix_1

matrix_2 <- matrix(abc_primes, nrow = 3, ncol = 1, byrow = TRUE)
matrix_2

###Lets test it out to get the standard deviation
matrix_1 %*% vcv_3x3_matrix %*% matrix_2
###this equals 0.001216933

##take the square root of 0.001216933
sqrt(0.001216933)
#equals 0.03488457 it matches the first one! So this is the standard error according to Jeff




###Another example code from Jeff
#Below I wrote a function in R that will compute the v-c matrix for an abitrary set of products of all the same length. x is the vector of estimates,
#vcv is the variance-covariance matrix of the estimates and indices is a matrix of indices in x for how you want to combine them. The number of rows
#in indices is the number of products and the number of columns is the number of estimates in each product.

dm.prod=function(x,vcv,indices)
{
  D=matrix(0,nrow=nrow(indices),ncol=prod(dim(indices)))
  for(i in 1:nrow(indices))
  {
    estimates=x[indices[i,]]
    D[i,indices[i,]]=prod(estimates)/estimates
  }
  return(D%*%vcv%*%t(D))
}

#Here is an example with the dipper data from above

# create 2 products with first 3 and last 3
indices=matrix(1:6,ncol=3,nrow=2,byrow=T)
# show indices
indices
# compute v-c matrix of the 2 products
vcv=dm.prod(rr$estimates$estimate,rr$vcv,indices=indices)
# show that std errors match the results above
sqrt(diag(vcv))

#You can expand to any number of products of any number of estimates by specifying a different index matrix.
# For example here is one that creates 3 products of each pair of estimates
indices=matrix(1:6,ncol=2,nrow=3,byrow=T)
indices
# Note indices do not have to be in order, just easier to do for the example
vcv=dm.prod(rr$estimates$estimate,rr$vcv,indices=indices)
vcv

