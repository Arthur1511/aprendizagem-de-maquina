rm(list=ls())
x=seq(-2,2,0.1)



# f1=sin(x)
# f2=cos(x)
f3=x
# 
# 
# a = -pi
# a1 = 0.565
# a2=2.657
# a3=0.674
# 
# 
# FX = a+a1*f1 + a2*f2 + a3*f3

Fx = (1-2*x^2)*exp(-(x^2))


par(mfrow = c(2, 2))

# plot(x,f1,type="l",main = "f(1)")
# plot(x,f2,type="l",main = "f(2)")
plot(x,f3,type="l",main = "f(3)")
# plot(x,FX,type="l",main = "f(x)")
plot(x,Fx,type="l",main = "f(x)")


source('C:/Users/arthu/Documents/Dev/trainadaline.R')
source('C:/Users/arthu/Documents/Dev/yadaline.R')

#------------------------------------------------------------
# Gerando dados para o treinamento

# xin<-cbind(f1,f2,f3)
# yd = as.matrix(FX)

xin<-cbind(x)
yd = as.matrix(Fx)

eta<-0.01 # Taxa de aprendizagem
tol<-0.0001 # Erro Tolerancia
maxepocas<-100 # Numero maximo de epocas
par<-2 # inserir o bias nos dados.


# Treinamento ----------------------------------------------------
peso_erro <- trainadaline(xin,yd,eta,tol,maxepocas,par)
w<- peso_erro[[1]]
erroepoc<- peso_erro[[2]]

par(mfrow = c(1, 1))
plot(erroepoc,type='l')

# FX_hat =  w[1] + w[2]*f1 + w[3]*f2 + w[4]*f3 
FX_hat = w[2]*x

# plot(x,Fx,type="l",col="red")
plot(x,FX_hat,type="o",col="blue")


