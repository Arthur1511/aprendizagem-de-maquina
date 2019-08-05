trainadaline <- function(xin,yd,eta,tol,maxepocas,par)
  # Trains a simple perceptron.
  # xin: Input Nxm dataset matrix.
  # eta: weight update step.
  # tol: error goal.
  # maxepocas: maximum number of epochs allowed.
  # par: par = 1 indicates that -1 needs to be agumented to xin
{
  dimxin<-dim(xin)       # Dimensions of the dataset.
  N<-dimxin[1]           # Number of patterns.
  m<-dimxin[2]           # Input dimension
                          
  if (par==1){           
    wt<-as.matrix(runif(m+1)-0.5) 
    xin<-cbind(1,xin)   
  } 
  else wt<-as.matrix(runif(m)-0.5)
      
  nepocas<-0
  eepoca<-tol+1
  evec<-matrix(nrow=1,ncol=maxepocas) # Initialize error vector.
  while ((nepocas < maxepocas) && (eepoca>tol))
  {
    ei2<-0
    xseq<-sample(N)       # Random sequence for training.
    for (i in 1:N)
    {
      irand<-xseq[i]      # Pattern from random sequence.
      #yhati<-as.double((xin[irand,] %*% wt) >= 0) # Model output.
      yhati<-as.double(xin[irand,] %*% wt) # Model output.
      ei<-yd[irand]-yhati    # Error
      dw<-eta*ei*xin[irand,]
      wt<-wt+dw              # Weight update
      ei2<-ei2+ei*ei         # Accumulate error.
    }
    nepocas<-nepocas+1       # Number of epochs.
    evec[nepocas]<-ei2/N     
    eepoca<-evec[nepocas]    # Error per epoch.
  
  }
  
  retlist<-list(wt,evec[1:nepocas])
  return(retlist)
}





