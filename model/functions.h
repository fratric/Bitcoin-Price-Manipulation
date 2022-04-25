#include <Rcpp.h>
using namespace Rcpp;

//---------------------------------------COMMON FUNCTIONS END-----------------------------------------------------

double mean(std::vector<double>& arr) { 
  double sum = 0.0; 
  int n = arr.size();
  for (int i = 0; i < n; i++){
    sum = sum + arr[i];         
  }
  return sum/n; 
} 


double standardDeviation(std::vector<double>& arr){ 
  double sum = 0.0; 
  int n = arr.size();
  double mean_est = mean(arr);
  for (int i = 0; i < n; i++){
    sum = sum + (arr[i] - mean_est) * (arr[i] - mean_est); 
  }
  return sqrt(sum/n); 
} 

double skewness(std::vector<double>& arr){
  double sum = 0.0; 
  int n = arr.size();
  double mean_est = mean(arr);
  double sd_est = standardDeviation(arr);
  for (int i = 0; i < n; i++){
    sum = sum + (arr[i] - mean_est)*(arr[i] - mean_est)*(arr[i] - mean_est);               
  }
  return sum/(n*sd_est*sd_est*sd_est); 
} 

void print_orders(std::vector<double>& df0, std::vector<double>& df1, std::vector<bool>& df2){
  if(df0.size() > 0){
    int n = df0.size();
    if(n > 5){
      n = 5;
    }
    for(int i = 0; i<n; i++){
      Rcout << df0.size() << " || " << df0[i] << " " << df1[i] << " " << df2[i] << "\n";
    }
  }else{
    Rcout << "no order \n";
  }
  Rcout << "\n";
}

void print_vector(Rcpp::NumericVector v){
  for(int i=0; i < v.length(); i++){
    Rcout << v[i] << " ";
  }
  Rcout << "\n";
}

void print_vector(std::vector<double> v){
  for(int i=0; i < v.size(); i++){
    Rcout << v[i] << " ";
  }
  Rcout << "\n";
}

void print_vector(std::vector<int> v){
  for(int i=0; i < v.size(); i++){
    Rcout << v[i] << " ";
  }
  Rcout << "\n";
}

List OB_plot(std::vector<double>& amounts, std::vector<double>& prices){
  double min_lp, max_lp;
  min_lp = prices[0];
  max_lp = prices.back();
  int order_type = -1;
  if(min_lp > max_lp){
    min_lp = prices.back();
    max_lp = prices[0];
    order_type = 1;
  }
  int n = 100;

  std::vector<double> res(n);
  double interval = (max_lp - min_lp)/n;
  std::vector<double> interval_vec(n);

  if(order_type == -1){
    int i = 0, j = 0;
    double b = min_lp + interval;
    interval_vec[0] = b;
    while(j < prices.size() && i < n){
      if(prices[j] <= b){
        res[i] = res[i] + amounts[j];
        j++;
      }else{
        i++;
        b = b + interval;
        interval_vec[i] = b;
      }
    }
  }else{
    double a = max_lp - interval;
    int i = 0, j = 0;
    interval_vec[0] = a;
    while(j < prices.size() && i < n){
      if(prices[j] >= a){
        res[i] = res[i] + amounts[j];
        j++;
      }else{
        i++;
        a = a - interval;
        interval_vec[0] = a;
      }
    }
  }
  List RetList;
  RetList["x"] = interval_vec;
  RetList["y"] = res;
  return(RetList);
}

//---------------------------------------COMMON FUNCTIONS END-----------------------------------------------------
//---------------------------------------MARKET FUNCTIONS START--------------------------------------------

double price_formation(double sell, double buy, double current_price){
  double pT;
  if(sell == 0 || buy == 0){
    if(buy > 0){
      pT = Rcpp::min(NumericVector({buy, current_price}));
    }
    if(sell > 0){
      pT = Rcpp::max(NumericVector({sell, current_price}));
    }
    if(sell == 0 && buy == 0){
      pT = current_price;
    }
  }else{
    pT = (buy+sell)/2.0;
  }
  return(pT);
}

NumericVector return_order(double residual_amount, double limit_price, bool isMMA, int expire){
  NumericVector order(4);
  order.names() = CharacterVector({"residual_amount","limit_price","isMMA","expire"});
  order[0] = residual_amount;
  order[1] = limit_price;
  order[2] = isMMA;
  order[3] = expire;
  return(order);
}

//only for already ordered vectors
int greatestLower(std::vector<double> v, double value){
  for(int i=v.size()-1;i>=0;i--){
    if(v[i] <= value){
      return(i);
    }
  }
  Rcout << "negative index in greatestLower" << "\n";
  return(0);
}

//only for already ordered vectors
int lowestGreater(std::vector<double> v, double value){
  for(int i=v.size()-1;i>=0;i--){
    if(v[i] >= value){
      return(i);
    }
  }
  Rcout << "negative index in lowestGreater" << "\n";
  return(0);
}

void AppendOrderSell(NumericVector this_order, std::vector<double>& dfSell_residual_amount, std::vector<double>& dfSell_limit_price, std::vector<bool>& dfSell_isMMA, std::vector<int>& dfSell_expire){
  if(dfSell_residual_amount.size() == 0){
    dfSell_residual_amount.push_back(this_order["residual_amount"]);
    dfSell_limit_price.push_back(this_order["limit_price"]);
    dfSell_isMMA.push_back(this_order["isMMA"]);
    dfSell_expire.push_back(this_order["expire"]);
  }else{
    if(dfSell_limit_price[0] > this_order["limit_price"] || this_order["limit_price"] == 0.0){
      if(dfSell_limit_price[0] == 0){
        int index = 0;
        for(int i = 0; i<dfSell_limit_price.size(); i++){
          if(dfSell_limit_price[0] == 0){
            index++;
          }else{
            break;
          }
        }
        dfSell_residual_amount.insert(dfSell_residual_amount.begin()+index,this_order["residual_amount"]);
        dfSell_limit_price.insert(dfSell_limit_price.begin()+index,this_order["limit_price"]);
        dfSell_isMMA.insert(dfSell_isMMA.begin()+index,this_order["isMMA"]);
        dfSell_expire.insert(dfSell_expire.begin()+index,this_order["expire"]);
      }else{
        dfSell_residual_amount.insert(dfSell_residual_amount.begin(),this_order["residual_amount"]);
        dfSell_limit_price.insert(dfSell_limit_price.begin(),this_order["limit_price"]);
        dfSell_isMMA.insert(dfSell_isMMA.begin(),this_order["isMMA"]);
        dfSell_expire.insert(dfSell_expire.begin(),this_order["expire"]);
      }
    }else{
      int r = greatestLower(dfSell_limit_price, this_order["limit_price"]);
      dfSell_residual_amount.insert(dfSell_residual_amount.begin() + r + 1,this_order["residual_amount"]);
      dfSell_limit_price.insert(dfSell_limit_price.begin() + r + 1,this_order["limit_price"]);
      dfSell_isMMA.insert(dfSell_isMMA.begin() + r + 1,this_order["isMMA"]);
      dfSell_expire.insert(dfSell_expire.begin() + r + 1,this_order["expire"]);
    }
  }
}

void AppendOrderBuy(NumericVector this_order, std::vector<double>& dfBuy_residual_amount, std::vector<double>& dfBuy_limit_price, std::vector<bool>& dfBuy_isMMA, std::vector<int>& dfBuy_expire){
  if(dfBuy_residual_amount.size() == 0){
    dfBuy_residual_amount.push_back(this_order["residual_amount"]);
    dfBuy_limit_price.push_back(this_order["limit_price"]);
    dfBuy_isMMA.push_back(this_order["isMMA"]);
    dfBuy_expire.push_back(this_order["expire"]);
  }else{
    if(dfBuy_limit_price[0] < this_order["limit_price"] || this_order["limit_price"] == 0.0){
      if(dfBuy_limit_price[0] == 0){
        int index = 0;
        for(int i = 0; i<dfBuy_limit_price.size(); i++){
          if(dfBuy_limit_price[0] == 0){
            index++;
          }else{
            break;
          }
        }
        dfBuy_residual_amount.insert(dfBuy_residual_amount.begin()+index,this_order["residual_amount"]);
        dfBuy_limit_price.insert(dfBuy_limit_price.begin()+index,this_order["limit_price"]);
        dfBuy_isMMA.insert(dfBuy_isMMA.begin()+index,this_order["isMMA"]); 
        dfBuy_expire.insert(dfBuy_expire.begin()+index,this_order["expire"]);
      }else{
        dfBuy_residual_amount.insert(dfBuy_residual_amount.begin(),this_order["residual_amount"]);
        dfBuy_limit_price.insert(dfBuy_limit_price.begin(),this_order["limit_price"]);
        dfBuy_isMMA.insert(dfBuy_isMMA.begin(),this_order["isMMA"]);
        dfBuy_expire.insert(dfBuy_expire.begin(),this_order["expire"]);
      }
    }else{
      int r = lowestGreater(dfBuy_limit_price, this_order["limit_price"]);
      dfBuy_residual_amount.insert(dfBuy_residual_amount.begin() + r + 1,this_order["residual_amount"]);
      dfBuy_limit_price.insert(dfBuy_limit_price.begin() + r + 1,this_order["limit_price"]);
      dfBuy_isMMA.insert(dfBuy_isMMA.begin() + r + 1,this_order["isMMA"]);
      dfBuy_expire.insert(dfBuy_expire.begin() + r + 1,this_order["expire"]);
    }
  }
}

void ExpireOrders(int max_len, std::vector<double>& df_residual_amount, std::vector<double>& df_limit_price, std::vector<bool>& df_isMMA, std::vector<int>& df_expire){
  int dfsize = df_residual_amount.size();
  if(dfsize > max_len){
    df_residual_amount.erase(df_residual_amount.begin() + max_len, df_residual_amount.end());
    df_limit_price.erase(df_limit_price.begin() + max_len, df_limit_price.end());
    df_isMMA.erase(df_isMMA.begin() + max_len, df_isMMA.end());
    df_expire.erase(df_expire.begin() + max_len, df_expire.end());
  }
  std::vector<int> tmp_vec(0);
  for(int i=0;i<dfsize;i++){
    if(df_expire[i] >= 0){
      tmp_vec.push_back(i);
    }
  }
  int tmp_len = tmp_vec.size();
  int nr_of_erased = 0;
  for(int i=0;i<tmp_len;i++){
    int to_erase = tmp_vec[i] - nr_of_erased;
    df_expire.erase(df_expire.begin() + to_erase);
    df_residual_amount.erase(df_residual_amount.begin() + to_erase);
    df_limit_price.erase(df_limit_price.begin() + to_erase);
    df_isMMA.erase(df_isMMA.begin() + to_erase);
    nr_of_erased++;
  }
  
  //just to check
  dfsize = df_residual_amount.size();
  for(int i=0;i<dfsize;i++){
    if(df_expire[i] > 0){
      Rcout << "Warning: unexpired order !"; 
    }
  }
}

void dayIncrement(std::vector<int>& df_expire){
  int dfsize = df_expire.size();
  for(int i=0;i<dfsize;i++){
    df_expire[i] = df_expire[i] + 1;
  }
}

std::vector<double> match_order(double current_price, std::vector<double>& dfSell_residual_amount, std::vector<double>& dfSell_limit_price, std::vector<bool>& dfSell_isMMA, std::vector<int>& dfSell_expire, std::vector<double>& dfBuy_residual_amount, std::vector<double>& dfBuy_limit_price, std::vector<bool>& dfBuy_isMMA, std::vector<int>& dfBuy_expire){
  std::vector<double> retList(4);
  retList[0] = current_price; //price
  retList[1] = 0; //executed 0=false, 1=true
  retList[2] = 0.0; //BTC_transfered_MMA
  retList[3] = 0.0; //BTC_transfered_total
  int dfSell_size = dfSell_residual_amount.size();
  int dfBuy_size = dfBuy_residual_amount.size();
  if(dfSell_size > 0 && dfBuy_size > 0){
    if(dfSell_limit_price[0] <= dfBuy_limit_price[0] || dfSell_limit_price[0] == 0 || dfBuy_limit_price[0] == 0){
      double pT = price_formation(dfSell_limit_price[0], dfBuy_limit_price[0], current_price);
      double residual = dfSell_residual_amount[0] - dfBuy_residual_amount[0];
      double minVal = Rcpp::min(Rcpp::NumericVector({dfSell_residual_amount[0],dfBuy_residual_amount[0]}));
      if(dfBuy_isMMA[0] == true || dfSell_isMMA[0] == true){ 
        retList[2] = minVal;
      }
      if(std::isnan(minVal) == true){
        Rcout << "is NAN in match_order !!"  << "\n";
      }
      retList[3] = minVal;
      if(residual > 0){
        dfSell_residual_amount[0] = dfSell_residual_amount[0] - dfBuy_residual_amount[0];
        dfBuy_residual_amount.erase(dfBuy_residual_amount.begin());
        dfBuy_limit_price.erase(dfBuy_limit_price.begin());
        dfBuy_isMMA.erase(dfBuy_isMMA.begin());
        dfBuy_expire.erase(dfBuy_expire.begin());
      }else if(residual < 0){
        dfBuy_residual_amount[0] = dfBuy_residual_amount[0] - dfSell_residual_amount[0];
        dfSell_residual_amount.erase(dfSell_residual_amount.begin());
        dfSell_limit_price.erase(dfSell_limit_price.begin());
        dfSell_isMMA.erase(dfSell_isMMA.begin());
        dfSell_expire.erase(dfSell_expire.begin());
      }else{
        dfBuy_residual_amount.erase(dfBuy_residual_amount.begin());
        dfBuy_limit_price.erase(dfBuy_limit_price.begin());
        dfBuy_isMMA.erase(dfBuy_isMMA.begin());
        dfBuy_expire.erase(dfBuy_expire.begin());
        dfSell_residual_amount.erase(dfSell_residual_amount.begin());
        dfSell_limit_price.erase(dfSell_limit_price.begin());
        dfSell_isMMA.erase(dfSell_isMMA.begin());
        dfSell_expire.erase(dfSell_expire.begin());
      }
      retList[1] = 1;
      retList[0] = pT;
    }
  }
  
  /*
  else if(dfSell_size == 0 || dfBuy_size == 0){
    if(dfSell_size == 0){
      double imaginary_price = current_price;
      if(imaginary_price <= dfBuy_limit_price[0] || imaginary_price == 0 || dfBuy_limit_price[0] == 0){
        double pT = price_formation(imaginary_price, dfBuy_limit_price[0], current_price);
        retList[3] = dfBuy_residual_amount[0];
        dfBuy_residual_amount.erase(dfBuy_residual_amount.begin());
        dfBuy_limit_price.erase(dfBuy_limit_price.begin());
        dfBuy_isMMA.erase(dfBuy_isMMA.begin());
        retList[1] = 1;
        retList[0] = pT;
      }
    }else if(dfBuy_size == 0){
      double imaginary_price = current_price;
      if(dfSell_limit_price[0] <= imaginary_price || dfSell_limit_price[0] == 0 || imaginary_price == 0){
        double pT = price_formation(dfSell_limit_price[0], imaginary_price, current_price);
        retList[3] = dfBuy_residual_amount[0];
        dfSell_residual_amount.erase(dfSell_residual_amount.begin());
        dfSell_limit_price.erase(dfSell_limit_price.begin());
        dfSell_isMMA.erase(dfSell_isMMA.begin());
        retList[1] = 1;
        retList[0] = pT;
      }
    }else{
      Rcout << "Error in matching";
    }
  }
  */
  
  return retList;
}

List keepMatching(double current_price, std::vector<double>& dfSell_residual_amount, std::vector<double>& dfSell_limit_price, std::vector<bool>& dfSell_isMMA, std::vector<int>& dfSell_expire, std::vector<double>& dfBuy_residual_amount, std::vector<double>& dfBuy_limit_price, std::vector<bool>& dfBuy_isMMA, std::vector<int>& dfBuy_expire){
  Rcpp::List retList;
  retList["BTC_transfered_MMA"] = 0.0;
  retList["BTC_transfered_total"] = 0.0;
  retList["new_price"] = current_price;
  
  double Issued;
  double transferedTotal_MMA = 0.0;
  double transferedTotal_total = 0.0;
  do{
    Issued = 0.0;
    std::vector<double> matchOrder = match_order(current_price, dfSell_residual_amount, dfSell_limit_price, dfSell_isMMA, dfSell_expire, dfBuy_residual_amount, dfBuy_limit_price, dfBuy_isMMA, dfBuy_expire);
    current_price = matchOrder[0];
    Issued = matchOrder[1];
    double transfered_MMA = matchOrder[2];
    double transfered_total = matchOrder[3];
    transferedTotal_MMA = transferedTotal_MMA + transfered_MMA;
    transferedTotal_total = transferedTotal_total + transfered_total;
    
  }while(Issued == 1.0);
  
  retList["new_price"] = current_price;
  retList["BTC_transfered_MMA"] = transferedTotal_MMA;
  retList["BTC_transfered_total"] = transferedTotal_total;
  return retList;
}


double return_cash_per_amount(double omega, std::vector<double>& df_limit_price, std::vector<double>& df_residual_amount){
  double cash = 0.0;
  int i = 0;
  bool run = true;
  int df_len = df_limit_price.size();
  while(run == true && df_len > 0){
    double last_price = df_limit_price[i];
    double offer = df_limit_price[i]*df_residual_amount[i];
    if(i < df_len){ 
      if(df_limit_price[i] == 0){
        Rcout << "warning: market order pending!";
      }
      if(omega - df_residual_amount[i] < 0){
        run = false;
      }else{
        omega = omega - df_residual_amount[i];
        cash = cash + offer;
        i = i + 1;
      }
    }else{
      Rcout << "warning: attempted to go beyond orderbook size!";
      double cash_to_add = omega*last_price;
      omega = 0; 
      cash = cash + cash_to_add;
      run = false;
    }
  }
  if(omega > 0.000001){
    double cash_to_add = omega*df_limit_price[i];
    cash = cash + cash_to_add;
  }
  return cash;
}


List return_amount_per_cash(double available_cash, std::vector<double>& df_limit_price, std::vector<double>& df_residual_amount, double current_price){
  double omega = 0.0, cash_spent = 0.0;
  int i = 0, min_df_len = 10;
  bool run = true;
  int df_len = df_limit_price.size();
  Rcpp::List ret;
  while(run == true && i <= df_len - min_df_len){
    double offer = df_limit_price[i]*df_residual_amount[i];
    if(df_limit_price[i] == 0){
      Rcout << "warning: market order pending!";
      offer = current_price*df_residual_amount[i];
    }
    if(available_cash - offer < 0){
      run = false;
    }else{
      available_cash = available_cash - offer;
      cash_spent = cash_spent + offer;
      omega = omega + df_residual_amount[i];
      i = i + 1;
      if(i == df_len - min_df_len){
        Rcout << "Warning: iterator in the order book above the limit \n";
      }
    }
  }
  
  //this is here in case there is still cash left but only for partial order on i-th level
  if(available_cash > 1 && available_cash < df_residual_amount[i]*df_limit_price[i]){ //more than 1 dollar left
    if(df_limit_price[i] == 0){
      Rcout << "warning: Dividing by zero!";
    }
    double btc_to_affort = available_cash/df_limit_price[i];
    omega = omega + btc_to_affort;
    cash_spent = cash_spent + btc_to_affort*df_limit_price[i];
  }
  

  if(i >= df_len){
    Rcout << "warning: MMA asking for entire order book (sell). df_len = " << df_len << "\n";
  }
  
  ret["btc_amount"] = omega;
  ret["cash_spent"] = cash_spent;
  return ret;
}

List return_amount_per_cash_buy(double available_cash, std::vector<double>& df_limit_price, std::vector<double>& df_residual_amount, double current_price){
  double omega = 0.0, cash_spent = 0.0;
  int i = 0, min_df_len = 50;
  bool run = true;
  int df_len = df_limit_price.size();
  Rcpp::List ret;
  while(run == true && i < df_len - 1){
    double offer = df_limit_price[i]*df_residual_amount[i];
    if(df_limit_price[i] == 0){
      Rcout << "warning: market order pending!";
      offer = current_price*df_residual_amount[i];
    }
    if(available_cash - offer < 0){
      run = false;
    }else{
      available_cash = available_cash - offer;
      cash_spent = cash_spent + offer;
      omega = omega + df_residual_amount[i];
      i = i + 1;
    }
  }
  
  //this is here in case there is still cash left but only for partial order on i-th level
  if(available_cash > 1 && available_cash < df_residual_amount[i]*df_limit_price[i]){ //more than 1 dollar left
    if(df_limit_price[i] == 0){
      Rcout << "warning: Dividing by zero!";
    }
    double btc_to_affort = available_cash/df_limit_price[i];
    omega = omega + btc_to_affort;
    cash_spent = cash_spent + btc_to_affort*df_limit_price[i];
    available_cash = available_cash - btc_to_affort*df_limit_price[i];
  }
  
  if(i >= df_len){
    Rcout << "warning: MMA asking for entire order book (buy). df_len = " << df_len << "\n";
  }
  
  ret["btc_amount"] = omega;
  ret["cash_spent"] = cash_spent;
  return ret;
}

List sell_strategy_generate(int monthIndex, Rcpp::IntegerVector eom_days, std::vector<double>& toProve, int Tt){
  Rcpp::List list;
  std::vector<int> days;
  std::vector<std::vector<int>> times;
  std::vector<std::vector<double>> cash;
  

  double toProve_month = toProve[monthIndex];
  int step_size = 1;
  int number_of_sell = Tt/step_size;
  if(number_of_sell > Tt || number_of_sell <= 0){
    Rcout << "invalide number_of_sell !!! \n";
  }
  
  // monthIndex == 0 only one day
  if(monthIndex == 1){
    std::vector<double> tmp_fraction = {0.5440995,0.4559005};
    for(int j = 0; j < tmp_fraction.size(); j++){
      days.push_back(eom_days[monthIndex] + j);
      std::vector<int> tmp_times;
      std::vector<double> tmp_cash;
      for(int i = 0; i < number_of_sell; i++){
        int time = i*step_size;
        tmp_times.push_back(time);
        double cash_val = tmp_fraction[j]*toProve_month/number_of_sell;
        tmp_cash.push_back(cash_val);
      }
      times.push_back(tmp_times);
      cash.push_back(tmp_cash);
    }
    
  }else if(monthIndex == 2){
    std::vector<double> tmp_fraction = {0.1362958, 0.1945955, 0.2200540, 0.2153293, 0.2337253};
    for(int j = 0; j < tmp_fraction.size(); j++){
      days.push_back(eom_days[monthIndex] + j);
      std::vector<int> tmp_times;
      std::vector<double> tmp_cash;
      for(int i = 0; i < number_of_sell; i++){
        int time = i*step_size;
        tmp_times.push_back(time);
        double cash_val = tmp_fraction[j]*toProve_month/number_of_sell;
        tmp_cash.push_back(cash_val);
      }
      times.push_back(tmp_times);
      cash.push_back(tmp_cash);
    }
  }else if(monthIndex == 3){
    std::vector<double> tmp_fraction = {0.4071453, 0.5928547};
    for(int j = 0; j < tmp_fraction.size(); j++){
      days.push_back(eom_days[monthIndex] + j);
      std::vector<int> tmp_times;
      std::vector<double> tmp_cash;
      for(int i = 0; i < number_of_sell; i++){
        int time = i*step_size;
        tmp_times.push_back(time);
        double cash_val = tmp_fraction[j]*toProve_month/number_of_sell;
        tmp_cash.push_back(cash_val);
      }
      times.push_back(tmp_times);
      cash.push_back(tmp_cash);
    }
    
  }else if(monthIndex == 4){
    std::vector<double> tmp_fraction = {0.5612051, 0.4387949};
    for(int j = 0; j < tmp_fraction.size(); j++){
      days.push_back(eom_days[monthIndex] + j);
      std::vector<int> tmp_times;
      std::vector<double> tmp_cash;
      for(int i = 0; i < number_of_sell; i++){
        int time = i*step_size;
        tmp_times.push_back(time);
        double cash_val = tmp_fraction[j]*toProve_month/number_of_sell;
        tmp_cash.push_back(cash_val);
      }
      times.push_back(tmp_times);
      cash.push_back(tmp_cash);
    }
    
  }else if(monthIndex == 5){
    std::vector<double> tmp_fraction = {0.3628918, 0.3922153, 0.2448929};
    for(int j = 0; j < tmp_fraction.size(); j++){
      days.push_back(eom_days[monthIndex] + j);
      std::vector<int> tmp_times;
      std::vector<double> tmp_cash;
      for(int i = 0; i < number_of_sell; i++){
        int time = i*step_size;
        tmp_times.push_back(time);
        double cash_val = tmp_fraction[j]*toProve_month/number_of_sell;
        tmp_cash.push_back(cash_val);
      }
      times.push_back(tmp_times);
      cash.push_back(tmp_cash);
    }
    
  }else{
    days.push_back(eom_days[monthIndex]);
    std::vector<int> tmp_times;
    std::vector<double> tmp_cash;
    for(int i = 0; i < number_of_sell; i++){
      int time = i*step_size;
      tmp_times.push_back(time);
      double cash_val = toProve_month/number_of_sell;
      tmp_cash.push_back(cash_val);
    }
    times.push_back(tmp_times);
    cash.push_back(tmp_cash);
  }
  
  
  
  list["days"] = days;
  list["times"] = times;
  list["cash"] = cash;
  return list;
}

/*
int regulation(Rcpp::NumericVector this_order, int regType){
  //double limit_price = this_order["limit_price"];
  double amount = this_order["residual_amount"];
  
  if(regType == 1){ // maximum order size
    double maximum = 0.01;
    if(amount > maximum){
      //Rcout << "warning maximum amount " << maximum << "exceeded by " << amount - maximum << "/m";
      return maximum;
    }
  }
  return amount;
}
*/ 
 