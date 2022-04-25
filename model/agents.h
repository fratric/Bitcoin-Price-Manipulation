#include <Rcpp.h>
//#include "functions.h"
using namespace Rcpp;

//--------------------------------------MARKET FUNCTIONS END----------------------------------------------
//--------------------------------------AGENT FUNCTIONS START---------------------------------------------

double rpareto(double alfa){
  return(exp(Rcpp::rexp(1,alfa)[0]));
}

double rbinorm(double p, double mean1, double sigma1, double mean2, double sigma2){
  if(p > Rcpp::runif(1)[0]){ //higher p means higher mode1
    return(Rcpp::rnorm(1,mean1, sigma1)[0]);
  }else{
    return(Rcpp::rnorm(1,mean2, sigma2)[0]);
  }
}

double return_limit(int order_type, double current_price, double meanN, double varN, std::vector<double>& dfSell_limit_price, std::vector<double>& dfBuy_limit_price){
  double price = current_price;
  
  double pmod = 0.40;
  
  if(Rcpp::runif(1)[0] < 0){
    price = 0;
  }else{
    if(order_type == 1){
      price = current_price/Rcpp::rnorm(1,meanN,varN)[0];
    }else if(order_type == -1){
      price = current_price*Rcpp::rnorm(1,meanN,varN)[0];
    }
  }
  return(price);
}
double return_limit2(int order_type, double current_price, double a, double b, double pleft, double pright, std::vector<double>& dfSell_limit_price, std::vector<double>& dfBuy_limit_price){
  double limit_price;
  double scaleWA = 0.5;
  double sigmascale = pright;
  
  if(order_type == -1){
    limit_price = current_price*(-pleft+pright)*Rcpp::rbeta(1,a,b)[0] + (1+pleft)*current_price;
  }else{
    limit_price = current_price*(-pleft+pright)*Rcpp::rbeta(1,b,a)[0] + (1-pright)*current_price;
  }
  return limit_price;
}

double BTCamount(double price, double current_price){
  double toReturn;
  if(Rcpp::runif(1)[0] < 0.05){
    toReturn = 0.5+0.5*Rcpp::rpois(1,1.0)[0];
  }else{
    toReturn = Rcpp::rexp(1,2.0)[0];
  }
  return(toReturn);
}  

int returnExpire(){
  int expire_date = Rcpp::rlnorm(1,1.5,0.1)[0];
  return(-1 - expire_date);
}


List random_trader(double current_price, double meanN, double varN, double a, double b, double pleft, double pright, std::vector<double>& dfSell_limit_price, std::vector<double>& dfBuy_limit_price){
  double limit_price;
  int order_type = 0;
  if(Rcpp::runif(1)[0] < 0.5){
    order_type = 1;
  }else{
    order_type = -1;
  }
  limit_price = return_limit(order_type, current_price, meanN, varN, dfSell_limit_price, dfBuy_limit_price);
  bool isMMA = false;
  double residual_amount = BTCamount(limit_price, current_price);
  int expire = returnExpire();
  
  List retList;
  retList["order_type"] = order_type;
  retList["order"] = return_order(residual_amount, limit_price, isMMA, expire);
  return(retList);
}


List chartist_long(double current_price, double meanN, double varN, double pCA_long, double prob_drop, double mean_long, std::vector<double>& dfSell_limit_price, std::vector<double>& dfBuy_limit_price, bool drop){
  int order_type=0;
  double limit_price, residual_amount;
  
  if(drop == true && Rcpp::runif(1)[0] < prob_drop){
    order_type = -1;
  }else if(Rcpp::runif(1)[0] < pCA_long){
      if(mean_long > 0){
        order_type = 1;
      }else{
        order_type = -1;
      }
  }

  limit_price = return_limit(order_type, current_price, meanN, varN, dfSell_limit_price, dfBuy_limit_price);
  residual_amount = BTCamount(limit_price, current_price);
  
  bool isMMA = false;
  int expire = returnExpire();
  List retList;
  retList["order_type"] = order_type;
  retList["order"] = return_order(residual_amount, limit_price, isMMA, expire);
  return(retList);
}



List whale_agent(double current_price, double a, double b, double pleft, double pright, std::vector<double>& dfBuy_limit_price, std::vector<double>& dfSell_limit_price){
  int order_type = 0;
  double limit_price, residual_amount = 0.0;
  
  order_type = 1;
  if(Rcpp::runif(1)[0] < 0.50){
    order_type = -order_type;
  }
  
  limit_price = return_limit2(order_type, current_price, a, b, pleft, pright, dfSell_limit_price, dfBuy_limit_price);
  residual_amount = BTCamount(limit_price, current_price);

  int expire = returnExpire();
  bool isMMA = false;
  List retList;
  retList["order_type"] = order_type;
  retList["order"] = return_order(residual_amount, limit_price, isMMA, expire);
  return(retList);
}


List LSE(std::vector<double>& dfBuy_residual_amount, std::vector<double>& dfSell_residual_amount, int day, int t, double percent){
  double limit_price, residual_amount;
  int order_type = 0;
  int freq = 25;
  //whale buy
  bool LSEb_bool = false;
  int index;
  Rcpp::IntegerVector LSEb = IntegerVector({333, 334, 341, 342, 343, 345});
  for(int i=0; i < LSEb.size(); i++){
    if(day == LSEb[i]){
      LSEb_bool = true;
      index = i;
    }
  }
  if(LSEb_bool == true){
    if(t % freq == 0){
      Rcpp::NumericVector LSEs_ratios = NumericVector({3.538633, 2.651711, 3.275189, 3.216667, 2.076381, 1.965138});
      double LSEs_scale = percent;
      residual_amount = LSEs_scale*LSEs_ratios[index];
      order_type = 1;
      limit_price = 0;
    }
  }
  
  //whale sell
  bool LSEs_bool = false;
  Rcpp::IntegerVector LSEs = IntegerVector({344, 356, 397, 401, 402});
  for(int i=0; i < LSEs.size(); i++){
    if(day == LSEs[i]){
      LSEs_bool = true;
      index = i;
    }
  }
  if(LSEs_bool == true){
    if(t % freq == 0){
      Rcpp::NumericVector LSEs_ratios = NumericVector({2.900676, 3.716152, 1.901109, 2.866988, 4.163546});
      
      double LSEs_scale = percent;
      residual_amount = LSEs_scale*LSEs_ratios[index];
      order_type = -1;
      limit_price = 0;
    }
  }

  int expire = returnExpire();
  bool isMMA = false;
  List retList;
  retList["order_type"] = order_type;
  retList["order"] = return_order(residual_amount, limit_price, isMMA, expire);
  return(retList); 
}

double return_toProve(std::vector<double>& cash_spent, int day, IntegerVector eom_days){
  double sumtotal = 0.0;
  int eom_len = eom_days.size();
  int tend = 0, tstart = 0;
  for(int i = 0; i < eom_len; i++){
    if(day == eom_days[i] && i > 0){
      tend = eom_days[i];
      tstart = eom_days[i-1];
    }else if(day == eom_days[i] && i == 0){
      tend = eom_days[i];
      tstart = 0;
    }
  }

  for(int i = tstart; i < tend; i++){
    sumtotal = sumtotal + cash_spent[i];
  }
  return sumtotal;
}


List market_controller(double current_price, int issue_time, int t, double MMAcash_perday, std::vector<double>& dfSell_residual_amount, std::vector<double>& dfSell_limit_price, std::vector<double>& dfBuy_residual_amount, std::vector<double>& dfBuy_limit_price, std::vector<double>& cash_spent, Rcpp::List sell_strategy, double& unproven){
  int order_type = -1;
  double btc = 0.0, residual_amount = 0.0, limit_price = 0.0;
  
  std::vector<int> days = sell_strategy["days"];
  std::vector<std::vector<int>> times = sell_strategy["times"];
  bool days_bool = false, times_bool = false;
  int days_index, times_index;
  
  for(int i = 0; i < days.size(); i++){
    if(days[i] == issue_time){
      days_index = i;
      days_bool = true;
      std::vector<int> times_per_day = times[i];
      for(int j = 0; j < times_per_day.size(); j++){
        if(times_per_day[j] == t){
          times_index = j;
          times_bool = true;
        }
      }
    }
  }
  
  if(days_bool == true && times_bool == true){
    std::vector<std::vector<double>> cash = sell_strategy["cash"];
    double sample_cash = cash[days_index][times_index];
    Rcpp::List btc_list = return_amount_per_cash(sample_cash, dfBuy_limit_price, dfBuy_residual_amount, current_price);
    double btc_amount = btc_list["btc_amount"];
    double cash_spent_per = btc_list["cash_spent"];
    unproven = unproven + sample_cash-cash_spent_per;
    btc = btc_amount;
  }else if(MMAcash_perday > 0.0){
    order_type = 1;
    double cash = MMAcash_perday;
    Rcpp::List btc_list = return_amount_per_cash_buy(cash, dfSell_limit_price, dfSell_residual_amount, current_price);
    double btc_amount = btc_list["btc_amount"]; 
    double cash_spent_per = btc_list["cash_spent"];
    btc = btc_amount;
    cash_spent[issue_time] = cash_spent[issue_time] + cash_spent_per;
  }else if(unproven > 0){
    order_type = 0;
  }else{
    order_type = 0;
  }
  if(btc <= 0.0){
    order_type = 0;
  }
  residual_amount = btc; 
  bool isMMA = true;
  int expire = returnExpire();
  List retList;
  retList["order_type"] = order_type;
  retList["order"] = return_order(residual_amount, limit_price, isMMA, expire);
  return(retList);
}


bool market_exit(double price){
  if(price < 50){
    return true;
  }
  return false;
}













