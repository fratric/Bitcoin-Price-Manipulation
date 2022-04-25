wd_path <- paste0(getwd(),"/")
icount <- 0

FUN <- function(x){
  
  MMAscale <- x[1]
  varN <- x[2]
  a <- x[3]
  pCA_long <- x[4]
  pRA <- x[5]
  pRSA <- x[6]
  prob_drop <- x[7]
  percent <- x[8]
  
  
  
  nr_days= length(activity$cashMMA)
  Tt = 1440
  meanN = 1.00
  b = 1.00
  pleft = 0.5
  pright = 0.015
  lShort = 6
  cashMMA = MMAscale*cashMatrix
  LSEbool = TRUE #0.08
  
  input <- list("nr_days"= nr_days,
                "Tt" = Tt,
                "meanN" = meanN,
                "varN" = varN,
                "a" = a,
                "b" = b,
                "pleft" = pleft,
                "pright" = pright,
                "lShort" = lShort,
                "cashMMA" = cashMMA,
                "prob_drop" = prob_drop,
                "pCA_long" = pCA_long, # increasing this worked well
                "percent" =  percent,
                "pRA" = pRA,
                "pRSA" = pRSA,
                "LSEbool" = LSEbool)
  
  nr_sim <- 16
  cl <- makeCluster(mc <- getOption("cl.cores", 8))
  clusterExport(cl=cl, varlist=c("nr_sim","input", "activity","MMAscale", "cashMatrix"))
  res <- parLapply(cl, seq_len(nr_sim), function(i){
    Rcpp::sourceCpp("simulator1.cpp")
    #input$cashMMA <- MMAscale*activity$cashMMA
    ext <- input
    ext$cashMatrix <- MMAscale*cashMatrix
    #print(paste0("totalcash = ",sum(input$cashMMA)))
    
    simulator1(TRUE,ext$nr_days,ext$Tt,ext$meanN,ext$varN,ext$a,ext$b,ext$pleft,ext$pright,ext$lShort,ext$cashMMA,ext$prob_drop,ext$pCA_long,ext$percent,ext$pRA,ext$pRSA,ext$LSEbool)
  })
  stopCluster(cl)
  index_vec <- c()
  nr_default <- 0
  for(i in 1:nr_sim){
    if(res[[i]]$default == TRUE){
      nr_default <- nr_default + 1
    }else{
      index_vec <- c(index_vec, i)
    }
  }
  nr_count <- length(index_vec)
  
  
  Mresults <- matrix(nrow = nr_count, ncol = nr_days)
  MresultsMarketVol <- matrix(nrow = nr_count, ncol = nr_days)
  index <- 1
  for(i in index_vec){
    Mresults[index,] <- res[[i]]$prices
    MresultsMarketVol[index,] <- res[[i]]$market_volume
    index <- index + 1
  }
  
  
  res <- data_frame("prices" = robustbase::colMedians(Mresults), "market_volume" = robustbase::colMedians(MresultsMarketVol))  
  err <- calc_error(res, nr_days)
  print(x)
  icount <- icount + 1
  print(paste0(icount, " error: ", err))
  
  if(is.numeric(err) && is.finite(err) && length(err) == 1){
    #print(err)
    return(err)
  }else{
    #print(err)
    #print(res)
    return(4000)
  }
}

lower <- c(0.001, 0.05, 2.10, 0.035, 0.250, 0.1, 0.007, 0.375)
upper <- c(0.002, 0.125, 3.0, 0.0425, 0.30, 0.2, 0.01, 0.525)
initial <- c(0.00135, 0.1, 2.2, 0.038, 0.2750, 0.12375, 0.00875, 0.450)

names(lower) <- c("MMAscale", "varN", "a", "pCA_long", "pRA", "pRSA", "prob_drop", "percent")
names(upper) <- c("MMAscale", "varN", "a", "pCA_long", "pRA", "pRSA", "prob_drop", "percent")
names(initial) <- c("MMAscale", "varN", "a", "pCA_long", "pRA", "pRSA", "prob_drop", "percent")

stosoo <- OOR::StoSOO(initial,
                      FUN,
                      lower = lower,
                      upper = upper,
                      nb_iter = 700,
                      control = list(type = "sto",
                                     light = FALSE))

saveRDS(stosoo, file = paste0(wd_path,"simulated_data/stosoo.rds"))
#stosoo <- readRDS(file = paste0(wd_path,"simulated_data/stosoo.rds"))

