yADALINE <- function(xvec,w,par)
# Returns the response y of a single Perceptron neuron.
# xvec: Input matrix with N rows and m colunms containing the 
#       dataset or a single input vector x, which may contain 
#       or not the additional fixed -1 element corresponding 
#       to the bias.
# w: Weight vector
# par: If par = 1 then x must be augmented by a -1 colunm.
  
{
  if (par==1)              # Checks par as above
    xvec<-cbind(1,xvec)   # Augments -1 if needed
  
    u<-xvec %*% w
    #y<-as.double((u>=0))
     y=u
  return((as.matrix(y)))
}
