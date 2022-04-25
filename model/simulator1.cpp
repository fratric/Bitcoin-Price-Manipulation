//#include <algorithm>
#include <Rcpp.h>
#include "functions.h"
#include "agents.h"
using namespace Rcpp;




// [[Rcpp::export]]
List simulator1(bool full_return, int Ndays, int Tt, double meanN, double varN, double a, double b, double pleft, double pright, int lShort, NumericMatrix matrixcash, double prob_drop, double pCA_long, double percent, double pRA, double pRSA, bool LSEbool) {

    //result variables and vectors
  Rcpp::NumericVector prices(Ndays);
  double current_price = 880.87;
  Rcpp::NumericMatrix activity_buy(3, Ndays);
  Rcpp::NumericMatrix activity_sell(3, Ndays);
  Rcpp::IntegerVector min_buyOrders(Ndays);
  Rcpp::IntegerVector min_sellOrders(Ndays);
  Rcpp::NumericVector MMA_inflow_perDay(Ndays);
  Rcpp::NumericVector Market_volume(Ndays);
  std::vector<double> bid_ask_spread(Tt*Ndays);
  std::vector<double> MMA_inflow(0);
  std::vector<double> cash_spent(Ndays);
  std::vector<double> toProve(6);
  int max_snap_size = 500;
  Rcpp::NumericMatrix snapshots_sell(Ndays, max_snap_size);
  Rcpp::NumericMatrix snapshots_buy(Ndays, max_snap_size);
  std::vector<double> slope(Ndays);
  Rcpp::NumericVector returns(Ndays);
  Rcpp::IntegerVector eom_days = IntegerVector({77, 145, 195, 257, 314, 381}); 
  int eom_days_len = eom_days.size();

  std::vector<double> dfSell_residual_amount(0);
  std::vector<double> dfSell_limit_price(0);
  std::vector<bool> dfSell_isMMA(0);
  std::vector<int> dfSell_expire(0);
  
  std::vector<double> dfBuy_residual_amount(0);
  std::vector<double> dfBuy_limit_price(0);
  std::vector<bool> dfBuy_isMMA(0);
  std::vector<int> dfBuy_expire(0);

  int order_book_max_len = 20000;

  //orderbook random initialization
  int warm_up = 10;
  Rcpp::NumericVector warmUp_prices(warm_up);
  for(int day=0;day<warm_up;day++){
    
    for(int t=0;t<Tt;t++){
      
      if(Rcpp::runif(1)[0] < pRA){
        List to_issue = random_trader(current_price, meanN, varN, a, b, pleft, pright, dfSell_limit_price, dfBuy_limit_price);
        if((int)to_issue["order_type"] != 0){
          if((int)to_issue["order_type"] == 1){
            AppendOrderBuy(to_issue["order"], dfBuy_residual_amount, dfBuy_limit_price,  dfBuy_isMMA, dfBuy_expire);
          }else if((int)to_issue["order_type"] == -1){
            AppendOrderSell(to_issue["order"], dfSell_residual_amount, dfSell_limit_price, dfSell_isMMA, dfSell_expire);
          }
          List tmpList = keepMatching(current_price, dfSell_residual_amount, dfSell_limit_price, dfSell_isMMA, dfSell_expire, dfBuy_residual_amount, dfBuy_limit_price, dfBuy_isMMA, dfBuy_expire);
          current_price = tmpList["new_price"];
        }
      }
      
      if(Rcpp::runif(1)[0] < pRSA){
        List to_issue = whale_agent(current_price, a, b, pleft, pright, dfBuy_limit_price, dfSell_limit_price);
        if((int)to_issue["order_type"] != 0){
          if((int)to_issue["order_type"] == 1){
            AppendOrderBuy(to_issue["order"], dfBuy_residual_amount, dfBuy_limit_price,  dfBuy_isMMA, dfBuy_expire);
          }else if((int)to_issue["order_type"] == -1){
            AppendOrderSell(to_issue["order"], dfSell_residual_amount, dfSell_limit_price, dfSell_isMMA, dfSell_expire);
          }
          List tmpList = keepMatching(current_price, dfSell_residual_amount, dfSell_limit_price, dfSell_isMMA, dfSell_expire, dfBuy_residual_amount, dfBuy_limit_price, dfBuy_isMMA, dfBuy_expire);
          current_price = tmpList["new_price"];
        }
      }
    }    
    dayIncrement(dfSell_expire);
    dayIncrement(dfBuy_expire);
    ExpireOrders(order_book_max_len, dfSell_residual_amount, dfSell_limit_price, dfSell_isMMA, dfSell_expire);
    ExpireOrders(order_book_max_len, dfBuy_residual_amount, dfBuy_limit_price, dfBuy_isMMA, dfBuy_expire);
    warmUp_prices[day] = current_price;
  }
  
  // prepare the variance of returns
  Rcpp::NumericVector history_short = warmUp_prices.import(warmUp_prices.end()-1-lShort, warmUp_prices.end()-1);
  Rcpp::NumericVector history_returns_short(lShort);
  for(int i=0;i<lShort;i++){
    history_returns_short[i] = (current_price/history_short[i]) - 1.0;
  }
  
  
  //simulation
  int index_bas = 0;
  int index_toProve = 0;
  int index_snaps = 0;
  double window_return, sl;
  bool drop = false;
  bool MMAdefault = false;
  int nrdrops = 0;
  double MMABTC_bought = 15.0, MMABTC_sold = 0.0, prooved = 0.0, unproven = 0.0;
  double MMABTC_balance = MMABTC_bought - MMABTC_sold;
  for(int day=0;day<Ndays;day++){
    activity_buy(0,day) = 0;
    activity_buy(1,day) = 0;
    activity_buy(2,day) = 0;
    activity_sell(0,day) = 0;
    activity_sell(1,day) = 0;
    activity_sell(2,day) = 0;
    
    min_buyOrders(day) = 1000;
    min_sellOrders(day) = 1000;
    MMA_inflow_perDay(day) = 0.0;
    Market_volume(day) = 0.0;
    double MMAinflow = 0.0;
    double market_volume = 0.0;

    Rcpp::List sell_strategy;
    std::vector<int> days(0);
    std::vector<std::vector<int>> times(0);
    std::vector<std::vector<double>> cash(0);
    sell_strategy["days"] = days;
    sell_strategy["times"] = times;
    sell_strategy["cash"] = cash;
    bool get_strategy = false;
    for(int i = 0; i < eom_days_len; i++){
      if(day == eom_days[i]){
        toProve[index_toProve] = return_toProve(cash_spent, day, eom_days);
        print_vector(toProve);
        Rcout << "unproven = " << unproven << " day = " << day << "\n"; 
        index_toProve = index_toProve + 1;
      }
    }
    if(index_toProve > 0){
      sell_strategy = sell_strategy_generate(index_toProve-1, eom_days, toProve, Tt);
    }
    
    int snapshop_t = Rcpp::sample(Tt-1,1)[0];
    int dfb_size = dfBuy_residual_amount.size();
    int dfs_size = dfSell_residual_amount.size();
    
    for(int t=0;t<Tt;t++){
      
      
      if(market_exit(current_price) == false && LSEbool == true){
        List to_issue = LSE(dfBuy_residual_amount, dfSell_residual_amount, day, t, percent);
        if((int)to_issue["order_type"] != 0){
          if((int)to_issue["order_type"] == 1){
            AppendOrderBuy(to_issue["order"], dfBuy_residual_amount, dfBuy_limit_price,  dfBuy_isMMA, dfBuy_expire);
          }else if((int)to_issue["order_type"] == -1){
            AppendOrderSell(to_issue["order"], dfSell_residual_amount, dfSell_limit_price, dfSell_isMMA, dfSell_expire);
          }
          List tmpList = keepMatching(current_price, dfSell_residual_amount, dfSell_limit_price, dfSell_isMMA, dfSell_expire, dfBuy_residual_amount, dfBuy_limit_price, dfBuy_isMMA, dfBuy_expire);
          current_price = tmpList["new_price"];
          double MMAincr = tmpList["BTC_transfered_MMA"];
          double totalincr = tmpList["BTC_transfered_total"];
          if((int)to_issue["order_type"] == -1){
            MMAinflow = MMAinflow + MMAincr;
            MMABTC_bought = MMABTC_bought + MMAincr;
          }else{
            MMABTC_sold = MMABTC_sold + MMAincr;
          }
          MMABTC_balance = MMABTC_bought - MMABTC_sold;
          market_volume = market_volume + totalincr;
        }
      }

      if(Rcpp::runif(1)[0] < pRA){
        List to_issue = random_trader(current_price, meanN, varN, a, b, pleft, pright, dfSell_limit_price, dfBuy_limit_price);
        if((int)to_issue["order_type"] != 0){
          if((int)to_issue["order_type"] == 1){
            AppendOrderBuy(to_issue["order"], dfBuy_residual_amount, dfBuy_limit_price,  dfBuy_isMMA, dfBuy_expire);
          }else if((int)to_issue["order_type"] == -1){
            AppendOrderSell(to_issue["order"], dfSell_residual_amount, dfSell_limit_price, dfSell_isMMA, dfSell_expire);
          }
          List tmpList = keepMatching(current_price, dfSell_residual_amount, dfSell_limit_price, dfSell_isMMA, dfSell_expire, dfBuy_residual_amount, dfBuy_limit_price, dfBuy_isMMA, dfBuy_expire);
          current_price = tmpList["new_price"];
          double MMAincr = tmpList["BTC_transfered_MMA"];
          if(MMAincr > 0.0){
            MMA_inflow.push_back(MMAincr); // MMAinflow zaznamena kolko MMA dostal za jeden nakup
          }
          double totalincr = tmpList["BTC_transfered_total"];
          if((int)to_issue["order_type"] == -1){
            MMAinflow = MMAinflow + MMAincr;
            MMABTC_bought = MMABTC_bought + MMAincr;
          }else{
            MMABTC_sold = MMABTC_sold + MMAincr;
          }
          MMABTC_balance = MMABTC_bought - MMABTC_sold;
          market_volume = market_volume + totalincr;
        }
      }

      if(MMABTC_balance < 0 || market_exit(current_price) == true){
        MMAdefault = true;
      }
      
      if(dfb_size > 0 && dfs_size > 0 && MMAdefault == false){
        double cashM = matrixcash(day,t);
        List to_issue = market_controller(current_price, day, t, cashM, dfSell_residual_amount, dfSell_limit_price, dfBuy_residual_amount, dfBuy_limit_price, cash_spent, sell_strategy, unproven);
        if((int)to_issue["order_type"] != 0){
          if((int)to_issue["order_type"] == 1){
            AppendOrderBuy(to_issue["order"], dfBuy_residual_amount, dfBuy_limit_price,  dfBuy_isMMA, dfBuy_expire);
            activity_buy(0,day) = activity_buy(0,day) + 1.0/Tt;
          }else if((int)to_issue["order_type"] == -1){
            AppendOrderSell(to_issue["order"], dfSell_residual_amount, dfSell_limit_price, dfSell_isMMA, dfSell_expire);
            activity_sell(0,day) = activity_sell(0,day) + 1.0/Tt;
          }
          List tmpList = keepMatching(current_price, dfSell_residual_amount, dfSell_limit_price, dfSell_isMMA, dfSell_expire, dfBuy_residual_amount, dfBuy_limit_price, dfBuy_isMMA, dfBuy_expire);
          current_price = tmpList["new_price"];
          double MMAincr = tmpList["BTC_transfered_MMA"];
          if(MMAincr > 0.0){
            MMA_inflow.push_back(MMAincr); 
          }
          double totalincr = tmpList["BTC_transfered_total"];
          if((int)to_issue["order_type"] == 1){
            MMAinflow = MMAinflow + MMAincr;
            MMABTC_bought = MMABTC_bought + MMAincr;
          }else{
            MMABTC_sold = MMABTC_sold + MMAincr;
          }
          MMABTC_balance = MMABTC_bought - MMABTC_sold;
          market_volume = market_volume + totalincr;
          
        }
      }

      if(dfb_size > 0 && dfs_size > 0  && current_price > 1 && market_exit(current_price) == false){

        double mean_long = history_returns_short[0];
        mean_long = mean(history_returns_short);
        
        if(current_price > 20000 && drop == false){
          drop = true;
        }
        if(drop == true && current_price < 10000){
          drop = false;
        }
        List to_issue = chartist_long(current_price, meanN, varN, pCA_long, prob_drop, mean_long, dfSell_limit_price, dfBuy_limit_price, drop);
        if((int)to_issue["order_type"] != 0){
          if((int)to_issue["order_type"] == 1){
            activity_buy(2,day) = activity_buy(2,day) + 1.0/Tt;
            AppendOrderBuy(to_issue["order"], dfBuy_residual_amount, dfBuy_limit_price,  dfBuy_isMMA, dfBuy_expire);
          }else if((int)to_issue["order_type"] == -1){
            activity_sell(2,day) = activity_sell(2,day) + 1.0/Tt;
            AppendOrderSell(to_issue["order"], dfSell_residual_amount, dfSell_limit_price, dfSell_isMMA, dfSell_expire);
          }
          List tmpList = keepMatching(current_price, dfSell_residual_amount, dfSell_limit_price, dfSell_isMMA, dfSell_expire, dfBuy_residual_amount, dfBuy_limit_price, dfBuy_isMMA, dfBuy_expire);
          current_price = tmpList["new_price"];
          double MMAincr = tmpList["BTC_transfered_MMA"];
          if(MMAincr > 0.0){
            MMA_inflow.push_back(MMAincr);
          }
          double totalincr = tmpList["BTC_transfered_total"];
          if((int)to_issue["order_type"] == -1){
            MMAinflow = MMAinflow + MMAincr;
            MMABTC_bought = MMABTC_bought + MMAincr;
          }else{
            MMABTC_sold = MMABTC_sold + MMAincr;
          }
          MMABTC_balance = MMABTC_bought - MMABTC_sold;
          market_volume = market_volume + totalincr;
        }
      }

      if(dfb_size > 0 && dfs_size > 0  && current_price > 1 && Rcpp::runif(1)[0] < pRSA){

        List to_issue = whale_agent(current_price, a, b, pleft, pright, dfBuy_limit_price, dfSell_limit_price);
        if((int)to_issue["order_type"] != 0){
          if((int)to_issue["order_type"] == 1){
            AppendOrderBuy(to_issue["order"], dfBuy_residual_amount, dfBuy_limit_price,  dfBuy_isMMA, dfBuy_expire);
          }else if((int)to_issue["order_type"] == -1){
            AppendOrderSell(to_issue["order"], dfSell_residual_amount, dfSell_limit_price, dfSell_isMMA, dfSell_expire);
          }
          List tmpList = keepMatching(current_price, dfSell_residual_amount, dfSell_limit_price, dfSell_isMMA, dfSell_expire, dfBuy_residual_amount, dfBuy_limit_price, dfBuy_isMMA, dfBuy_expire);
          current_price = tmpList["new_price"];
          double MMAincr = tmpList["BTC_transfered_MMA"];
          double totalincr = tmpList["BTC_transfered_total"];
          if((int)to_issue["order_type"] == -1){
            MMAinflow = MMAinflow + MMAincr;
            MMABTC_bought = MMABTC_bought + MMAincr;
          }else{
            MMABTC_sold = MMABTC_sold + MMAincr;
          }
          MMABTC_balance = MMABTC_bought - MMABTC_sold;
          market_volume = market_volume + totalincr;
        }
      }
      dfb_size = dfBuy_residual_amount.size();
      dfs_size = dfSell_residual_amount.size();

      
      if(min_buyOrders(day) > dfb_size){
        min_buyOrders(day) = dfb_size;
      }
      if(min_sellOrders(day) > dfs_size){
        min_sellOrders(day) = dfs_size;
      }
      
      
      if(dfs_size > 0 && dfb_size > 0){
        bid_ask_spread[index_bas] = 0.5*(dfSell_limit_price[0] + dfBuy_limit_price[0]);
      }else{
        bid_ask_spread[index_bas] = 0.0;
      }
      index_bas++;
      
      if(full_return == true){
        slope[day] = sl;
        
        if(dfb_size > max_snap_size && dfs_size > max_snap_size && snapshop_t == t){
          double mid_price = 0.5*(dfSell_limit_price[0] + dfBuy_limit_price[0]);
          for(int j = 0; j < max_snap_size; j++){
            snapshots_sell(day,j) = (dfSell_limit_price[j] - mid_price)/mid_price;
            snapshots_buy(day,j) = (dfBuy_limit_price[j] - mid_price)/mid_price;
          }
        }
      }
      
      if(current_price <= 1.0 || dfs_size == 0 || dfb_size == 0){
        current_price = 1.0;
        
      }
    }

    MMA_inflow_perDay(day) = MMAinflow;
    Market_volume(day) = market_volume;
    
    dayIncrement(dfSell_expire);
    dayIncrement(dfBuy_expire);
    ExpireOrders(order_book_max_len, dfSell_residual_amount, dfSell_limit_price, dfSell_isMMA, dfSell_expire);
    ExpireOrders(order_book_max_len, dfBuy_residual_amount, dfBuy_limit_price, dfBuy_isMMA, dfBuy_expire);

    prices[day] = current_price;
    history_short.erase(history_short.begin());
    history_short.push_back(prices[day]);
    for(int i=0; i<lShort;i++){
      history_returns_short[i] = (prices[day]/history_short[i]) - 1.0;
    }
    returns(day) = MMABTC_balance;
    
    
  }
  //stop:
  Rcout << "MMA balance = " << MMABTC_balance << "\n";
  Rcout << "unproven at the end = " << unproven << "\n"; 

  
  Rcpp::List ret;
  ret["prices"] = prices;
  ret["MMAinflow_perDay"] = MMA_inflow_perDay;
  ret["market_volume"] = Market_volume;
  if(full_return == true){
    ret["activity_buy"] = activity_buy;
    ret["activity_sell"] = activity_sell;
    ret["min_buyOrders"] = min_buyOrders;
    ret["min_sellOrders"] = min_sellOrders;
    ret["MMAinflow"] = MMA_inflow;
    ret["bid_ask_spread"] = bid_ask_spread;
    ret["snapshots_sell"] = snapshots_sell;
    ret["snapshots_buy"] = snapshots_buy;
    ret["slope"] = slope;
    ret["BTCamount"] = returns;
    ret["default"] = MMAdefault;
  }
  return(ret); 
}


/*
 double scale2 = scaleCA;
 
 bool up=false,down=false;
 
 if(scale2*window_return > Rcpp::rnorm(1,thCA,sigma)[0]){
 up = true;
 }
 if(scale2*window_return < Rcpp::rnorm(1,-thCA,sigma)[0]){
 down = true;
 }
 
 limit_price = return_limit(order_type, current_price, a, b, pleft, pright, dfSell_limit_price, dfBuy_limit_price);
 if(down == true && up == false){
 order_type = -1;
 }else if(up == true && down == false){
 order_type = 1;
 }else if(up == true && down == true){
 if(Rcpp::runif(1)[0] < 0.5){
 order_type = 1;
 }else{
 order_type = -1;
 }
 }else{
 order_type = 0;
 }
 residual_amount = BTCamount(limit_price, current_price);
 if(Rcpp::runif(1)[0] > pCA){
 order_type = 0;
 }
*/

/*
 *   if(scaleWA*window_return <= Rcpp::rnorm(1,0.0,sigma)[0]){
 //if(window_return >= 0){
 order_type = 1;
 limit_price = return_limit(order_type, current_price, a, b, pleft, pright, dfSell_limit_price, dfBuy_limit_price);
 }else{
 order_type = -1;
 limit_price = return_limit(order_type, current_price, a, b, pleft, pright, dfSell_limit_price, dfBuy_limit_price);
 }
 residual_amount = BTCamount(limit_price, current_price);
 
 
 */