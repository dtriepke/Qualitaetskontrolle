
library(shiny)
library(dplyr)
library(plotly)

server <- function(input, output) {
  
  # Function for generating operatin characteristic
  L_p_func <- function(N, n, c, p, print = TRUE){
    
    # PART: sanity check part------
    stopifnot(p <= 1 & p >= 0 ) # check whether p is element (0,1)
    for (x in c(N,n,c)){
      # N, n, c must be a integer/ double
      stopifnot(abs(x - round(x)) < .Machine$double.eps^0.5)
    }
    
    # PART: claculation --------
    p_range <- p # p can be a scalar or a set
    L <- 0 # forst entry is abitrarly number and will be removed after loop
    
    # forst loop takes the stated subspace od the definition range
    for (p in p_range){
      # calc M regarding p = M/N
      M <- round(p*N, 0)
      
      # second loop calc a for a given p the operation characteristic
      tmp <- 0 # temporary storage
      for ( i in c(0:c)){
        tmp <- tmp + dhyper(i, M, N - M, n)
      } 
      L <- c(L, tmp) # storage the result for L in a vector
    }
    
    # PART: output settings -----
    output <- list(L = L[-1], p = p_range) 
    if(print){cat(paste0("L(",p_range,") = ", paste0(round(output$L,5), ";  ") ))}
    return(output)
  }
  
  find_n_c <- function(n = 1, c = 0, 
                       alpha, 
                       AQL, # p_{1- alpha}
                       beta, 
                       RQL, # p_{beta}
                       N){
    
    `1-alpha` <- 1 - alpha
    L_AQL <- L_p_func(N = N, n = n, c = c, p = AQL, print = FALSE)
    L_RQL <- L_p_func(N = N, n = n, c = c, p = RQL, print = FALSE)
    
    if(L_AQL$L >= `1-alpha` & L_RQL$L <= beta){
      return(list(n = n, c = c))
    }
    
    while(L_RQL$L > beta){
      n <- n + 1
      L_RQL <- L_p_func(N = N, n = n, c = c, p = RQL, print = FALSE)
    }
    
    L_AQL <- L_p_func(N = N, n = n, c = c, p = AQL, print = FALSE)
    
    
    if(L_AQL$L >= `1-alpha`){
      return(list(n = n, c = c))
    }
    
    c <- c + 1
    find_n_c(n = n, c = c, alpha, AQL, beta, RQL, N)
    
  }
  
  optPlan <- reactive(quote({
    input$action
    tmp <- isolate(find_n_c(n = 1, c = 0, alpha  = 1 - input$one_min_alpha, AQL = input$AQL, beta = input$beta, RQL = input$RQL, N = input$N))
    as.data.frame(tmp)
  }), quoted = TRUE)
  
  
  output$plot <- renderPlotly({
    input$action
    df <- optPlan()
    n <- df$n
    c <- df$c
    L_p <- isolate(L_p_func(N = input$N, n = n, c = c, p = seq(0,1, by = 0.01), print = FALSE))
    
    isolate(plot_ly(x = L_p$p, y = L_p$L, type = "scatter", mode = "lines", name = "L(p)") %>% 
              layout(xaxis = list(title = "p"),
                     yaxis = list(title = "L(P)")) %>% 
              add_trace(x = c(input$AQL, input$AQL), y = c(0,1), type = "scatter", mode = "lines", name = "AQL") %>% 
              add_trace(x = c(input$RQL, input$RQL), y = c(0,1), type = "scatter", mode = "lines", name = "RQL") %>% 
              add_trace(x = c(input$AQL), y = c(input$one_min_alpha), type = "scatter", mode = "markers", name = "L(p_1-alpha)") %>% 
              add_trace(x = c(input$RQL), y = c(input$beta), type = "scatter", mode = "markers", name = "L(p_beta)"))
  })
  
  output$text <- renderText({
    input$action
    df <- optPlan()
    isolate(paste("n*:", df$n, "  c*:", df$c))
    
  })
}

shinyServer(server)
