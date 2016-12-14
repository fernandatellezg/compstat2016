#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(invgamma)

shinyServer(function(input, output) {
####################################################### simulaciones ###################################
     
      #defino el vector con simulaciones. 
      vector_sims <- function(){
        return((-log(1-runif(input$num_sim_exp))/(input$theta)))
      }
      
      output$histograma_sims<-renderPlot (  {
          hist(vector_sims(),
               breaks=input$num_bins, col='blue',
               xlab="Valor",
               ylab="Densidad",
               probability = TRUE,
               main= "Distribucion exponencial por metodo de la funcion inversa")
           curve(dexp(x, rate=input$theta),add=TRUE) } #llavesita renderplot
      ) #cierro renderplot
      
      output$vector_sims <- renderTable(vector_sims())
      
    prueba<-reactive({
        prueba_exponencial<- ks.test(vector_sims(),dexp)
        prueba_exponencial
    })
    
    output$ks<-renderPrint(prueba())

#####################################################################################################################################

    phi<- reactive({
        texto<-paste("aux<-", input$Phi)
        eval(parse(text=texto))
        aux
    })
    
    output$graficamontecarlo<- renderPlot({
        nsim<-input$num_sim_int_MC
        nmin<-input$num_min
        nmax<-input$num_max
        a<-input$a
        b<-input$b
        alpha<-input$alpha

        mc<-replicate(nsim,{
            n<-floor(runif(1,nmin,nmax))
            densx<-runif(n,min=a,max=b)
            phix<-sapply(densx,phi())
            estim<-mean(phix)
            intervalo<-estim + c(-1,1)*qnorm(1-alpha/2)*sqrt(var(phix)/n)
            (c(n,estim,intervalo[1],intervalo[2]))
        }) # funcion mc
        
        mc<-as.data.frame(t(mc))
        names(mc)<-c("Nsimulaciones", "Estimacion", "int_inf","int_sup")
        mc
        mc_sims<<-mc
        graf<- ggplot(mc, aes(x=Nsimulaciones,y=Estimacion)) +
            ggtitle(paste0("Estimacion Montecarlo con ",(1-alpha)*100,"% de confianza")) +
            geom_line(aes(y = Estimacion), colour = "red") +
            geom_ribbon(aes(ymin = int_inf, ymax= int_sup),alpha=0.2)
        graf

    }) #output grafica monte carlo
    
    output$est_montecarlo <- renderText({
      nsim<-input$num_sim_int_MC
      nmin<-input$num_min
      nmax<-input$num_max
      a<-input$a
      b<-input$b
      alpha<-input$alpha
      
      mc<-replicate(nsim,{
        n<-floor(runif(1,nmin,nmax))
        densx<-runif(n,min=a,max=b)
        phix<-sapply(densx,phi())
        estim<-mean(phix)
        intervalo<-estim + c(-1,1)*qnorm(1-alpha/2)*sqrt(var(phix)/n)
        (c(n,estim,intervalo[1],intervalo[2]))
      }) # funcion mc
      
      mc<-as.data.frame(t(mc))
      names(mc)<-c("Nsimulaciones", "Estimacion", "int_inf","int_sup")
      mc_media<-mean(mc[,2])
      as.character(mc_media)
      
    }) #output estimaci??n Montecarlo
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################

    df <- reactive({
      f <- data.frame(comisiones=c(3.6,5.2,5.3,7.3,5,5.2,3,3.1,3.2,7.5,8.3,6.1,4.9,5.8,7.11),ventas=c(11.28,14.74,18.46,20.01,12.43,15.37,9.59,11.26,8.05,27.91,24.62,18.8,13.87,12.11,23.68))
      f
    })
    
    data <- reactive({
      f <- data.frame(comisiones=c(3.6,5.2,5.3,7.3,5,5.2,3,3.1,3.2,7.5,8.3,6.1,4.9,5.8,7.11),ventas=c(11.28,14.74,18.46,20.01,12.43,15.37,9.59,11.26,8.05,27.91,24.62,18.8,13.87,12.11,23.68))
      as.matrix(f)
    })
    
    output$data <- renderTable({
      data()
    })
    
    output$vdui <- renderUI({
      selectInput('vd', label = 'Variable dependiente', names(df()))
    })
    
    output$viui <- renderUI({
      selectInput('vi', label = 'Variable independiente', names(df()))
    })
    
    output$varpt <- renderPlot({
      if(!is.null(df))
      {
        y <- df()[[input$vd]]
        x <- df()[[input$vi]]
        plot(x,y)
      }
    })
    
    nreg <- reactive({
      equis<-df()[[input$vi]]
      ye <- df()[[input$vd]]
      sp<-data.frame(equis,ye)
      splm<-lm(ye~equis,data=sp)
      summary_splm<-summary(splm)
      betas<-coefficients(summary_splm)
      list('betas' = betas, 'summary' = summary_splm)
    })
    
    ndist <- reactive({
      x <- seq(-100, 100, length=100)
      dnorm(x,round(nreg()$betas[1,1],digits=2),round(nreg()$betas[1,2],digits=2))
    })
    
    gdist <- reactive({
      x <- seq(-100, 100, length=100)
      dinvgamma(x,13.5,round(25*nreg()$summary$sigma,digits=2))
    })
    
    output$apra <- renderPlot({
      x <- seq(-100, 100, length=100)
      plot(x, ndist(), type="l", lty=2, xlab="x value",ylab="Density", main=paste('A priori a'))
    })
    
    output$aprb <- renderPlot({
      x <- seq(-100, 100, length=100)
      plot(x, ndist(), type="l", lty=2, xlab="x value",ylab="Density", main=paste('A priori b'))
    })
    
    output$aprg <- renderPlot({
      x <- seq(-100, 100, length=100)
      plot(x, gdist(), type="l", lty=2, xlab="x value",ylab="Density", main=paste('A priori eps'))
    })
    
    
    likelihood <- function(param){
      b1= param[1]
      b0 = param[2]
      sigma2 = param[3]
      pred = b1*df()[[input$vi]] + b0
      singlelikelihoods = dnorm(df()[[input$vd]], mean = pred, sd = sigma2**.5, log = T)
      sumll = sum(singlelikelihoods)
      return(sumll)
    }
    
    prior <- function(param){
      b1 = param[1]
      b0 = param[2]
      sigma2 = param[3]
      b1prior = dnorm(b1, mean=round(nreg()$betas[1,1],digits=2), sd=round(nreg()$betas[1,2]**.5,digits=2), log = T)
      b0prior = dnorm(b0, mean=round(nreg()$betas[2,1],digits=2), sd=round(nreg()$betas[2,2]**.5,digits=2), log = T)
      sigma2prior = dinvgamma(sigma2,14,round(25*nreg()$summary$sigma,digits=2),log = T)
      return(b1prior+b0prior+sigma2prior)
    }
    
    posterior <- function(param){
      return (likelihood(param) + prior(param))
    }
    
    #Metropolis
    
    proposalfunction <- function(param){
      return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
    }
    
    run_metropolis_MCMC <- function(startvalue, iterations){
      chain <- array(dim = c(iterations+1,3))
      chain[1,] <- startvalue
      for (i in 1:iterations){
        proposal <- proposalfunction(chain[i,])
        
        logprobab =posterior(proposal) - posterior(chain[i,])
        if (log(runif(1)) <= logprobab){
          chain[i+1,] = proposal
        }else{
          chain[i+1,] = chain[i,]
        }
      }
      return(chain)
    }
    
    mcmc <- reactive({
      startvalue = c(rnorm(1,0,1),rnorm(1,0,1),rinvgamma(1,1,1))
      chain = run_metropolis_MCMC(startvalue, input$long_cadenas)
      data.frame(b1=chain[,1],b0=chain[,2],s2=chain[,3])
    })
    
    output$sim <- renderDataTable({
      mcmc()
    })
    
    output$histos<-renderPlot({
      burnIn = input$long_cadenas*.20
      acceptance = 1-mean(duplicated(mcmc()[-(1:burnIn),]))
      par(mfrow = c(2,3))
      hist(mcmc()[-(1:burnIn),1],nclass=30,  main="Posterior of b1", xlab="Parametro" )
      abline(v = mean(mcmc()[-(1:burnIn),1]))
      hist(mcmc()[-(1:burnIn),2],nclass=30, main="Posterior of b0", xlab="Parametro")
      abline(v = mean(mcmc()[-(1:burnIn),2]))
      hist(mcmc()[-(1:burnIn),3],nclass=30, main="Posterior of sigma^2", xlab="Parametro")
      abline(v = mean(mcmc()[-(1:burnIn),3]) )
      plot(mcmc()[-(1:burnIn),1], type = "l", xlab="Iteraciones" , main = "Chain values of b1" )
      plot(mcmc()[-(1:burnIn),2], type = "l", xlab="Iteraciones" , main = "Chain values of b0")
      plot(mcmc()[-(1:burnIn),3], type = "l", xlab="Iteraciones" , main = "Chain values of sigma^2")
    })
    

}) #shiny server function

