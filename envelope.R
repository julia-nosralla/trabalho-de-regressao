#*************************************************************************Description*************************************************************************************************#
# Cria gráficos de probabilidade normal dos resíduos Studentizados com envelope simulado para avaliar a qualidade do ajuste da regressão linear normal sob mínimos quadrados
#***ARGUMENTS***#
# fit - um objeto contendo os resultados obtidos via a função lm().
# main.title - Título do gráfico. Padrão é "Envelope".
# faixa.fixed - limites dos resíduos (opcional). Padrão é NULL indicando que os limites são os valores mínimo e máximo dos resíduos.
# number.id - Número de observações que serão destacadas no gráfico (opcional). Padrão é NULL indicando que nenhuma das observações será identificada no gráfico.
#**************************************************************************************************************************************************************************************#

envelope_LR <- function(fit, main.title = "Envelope", faixa.fixed = NULL, number.id = NULL) { 
B <- 100; #number of replicates
X <- model.matrix(fit)
p <- ncol(X);  n <- nrow(X)

#***parameters for parametric bootstrap***#

Menvelope_r <- matrix(numeric(0),nrow=n,ncol=B)

#------------> residuals for the observed sample<--------------#
ts <- rstudent(fit) #Studentized residuals
betahat <- as.numeric(fit$coefficients)
sigma2hatc <- sum(resid(fit)^2)/(n-p) #sigma2 estimate (constant)
sigma2hat <- rep(sigma2hatc,n)

 for(j in 1:B){		        
     ygen <- rnorm(n, X%*%betahat, sqrt(sigma2hat)) 
     fitb <- lm(ygen~X[,2:p])
     Res <- rstudent(fitb)
     Menvelope_r[,j] = Res
    }
Menvelope <- apply(Menvelope_r,2,sort);          
res <-    ts;    
res_min  <-    as.numeric(t(apply(t(Menvelope), 2,quantile, probs =0.05)));         
res_mean <-    as.numeric(t(apply(t(Menvelope), 2,quantile, probs =0.5)));                              
res_max  <-    as.numeric(t(apply(t(Menvelope), 2,quantile, probs =0.95)));           
faixa <- range(res,res_min,res_max)
if(is.vector(faixa.fixed)) faixa <- faixa.fixed
par(mar=c(5.0,5.0,4.0,2.0))
v <- qqnorm(res, main=main.title, xlab="Percentil esperado via N(0,1)", ylab="Resíduos Studentizados", ylim=faixa, pch=16, cex=1, cex.lab=2.0, cex.axis=1.5, cex.main=2.0)
if(is.numeric(number.id)){
identify(v$x,v$y,cex =1.3, n=number.id) #identify points in the plot
}
par(new=T)
#
qqnorm(res_min,axes=F,main = "",xlab="",ylab="",type="l",ylim=faixa,lty=1,lwd=2.0)
par(new=T)
qqnorm(res_max,axes=F,main = "",xlab="",ylab="", type="l",ylim=faixa,lty=1,lwd=2.0)
par(new=T)
qqnorm(res_mean,axes=F,xlab="",main = "", ylab="", type="l",ylim=faixa,lty=2,lwd=2.0)
}#ends function
