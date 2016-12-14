
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(invgamma)


# Define UI for application that draws a histogram
shinyUI(fluidPage(title="Estadistica Computacional",
                  tabsetPanel(
                      tabPanel("Simulacion por funcion inversa",
                               sidebarLayout(
                                   sidebarPanel(
                                       h4("Simulacion de una distribucion exponencial por el metodo de la funcion inversa"),
                                       numericInput(inputId="num_sim_exp",label="Numero de simulaciones",
                                                    value=1000, min=0, max=100000),#si no se pone el m??ximo el vector pesa demasiado
                                       numericInput(inputId="theta", label="Parametro Theta de la distribucion (La esperanza de la distribucion exponencial es 1/theta)",
                                                    value=1,min=0,max=10000),
                                       numericInput(inputId="num_bins", label="Numero de bins en el histograma",
                                                    value=35,min=0,max=500)
                                   ),
                                   mainPanel(
                                       plotOutput("histograma_sims"),
                                       textOutput("ks"),
                                       tableOutput("vector_sims")

                                   )
                               )
                      ),

                      tabPanel("Montecarlo",
                               sidebarLayout(
                                   sidebarPanel(
                                       h4("Integracion numerica usando Montecarlo"),
                                       textInput(inputId="Phi", label="Funcion a integrar", value= "function(x) 2*x"),
                                       numericInput(inputId = "a", label="Limite inferior de integracion", value=0),
                                       numericInput(inputId = "b", label="Limite superior de integracion", value=1),
                                       numericInput(inputId="num_sim_int_MC", label="Numero de puntos a simular", value=100),
                                       numericInput(inputId = "num_min", label="Numero de simulaciones minimo", value= 100),
                                       numericInput(inputId = "num_max", label="Numero de simulaciones maximo", value= 10000),
                                       sliderInput(inputId = "alpha", label="Alpha del nivel de confianza (1-Alpha= nivel de confianza)", min= .001, max=.1, value=0.05)
                                       ),
                                   mainPanel(
                                       plotOutput("graficamontecarlo"),
                                       h6("El valor de la integral entre estos limites es:   "),
                                       textOutput("est_montecarlo")
                                   )
                               )
                      ),       
                        tabPanel("Regresion con Metropolis Hastings",
                                        sidebarLayout(
                                            sidebarPanel( 
    
                                              selectInput("vd", "Variable Dependiente:", choices = c('Ventas netas'='ventas','Comisiones pagadas'='comisiones')), 
                                              #checkboxGroupInput("checkGroup", label = "Variables Independientes", 
                                              #                   choices = c('Sepal.Length'=1,'Sepal.Width'=2,'Petal.Length'=3,'Petal.Width'=4),
                                              #                   selected = 2:4),
                                              selectInput("vi", "Variable Independiente:", choices = c('Comisiones pagadas'='comisiones','Ventas netas'='ventas')),
                                              sliderInput('cadenas', label = 'Numero de cadenas', value = 1, min = 1, max = 5, step = 1),
                                              sliderInput('long_cadenas', label = 'Longitud de la cadena', min = 1000, max = 100000, value =1000, step = 1000)
                                              #sliderInput('burn_in', label = 'Burn-in', min = 100, max = 1000, value =100, step = 100)
                                            ), 
                                            mainPanel(
                                              plotOutput('histos'),
                                              br(),
                                              plotOutput('apra'),
                                              plotOutput('aprb'),
                                              plotOutput('aprg'),
                                              br(),
                                            
                                              p('Resultados simulacion'),
                                              dataTableOutput('sim'),
                                              textOutput('test')

                                            )

                                        )
                               )

                      
                  ))

)
