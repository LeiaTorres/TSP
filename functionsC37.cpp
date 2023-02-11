#include <Rcpp.h>
using namespace Rcpp;

#include <unordered_map>

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector df_to_matrix(DataFrame df){
  NumericVector v1 = df[0];
  NumericVector v2 = df[1];
  NumericVector dis = df[2];
  int n = v1.size();
  //NumericVector v(2,1000);
  NumericMatrix M(n);
  
  for(int i = 0; i < n; i++){
    for(int j = 0; j < n; j++){
      M(i,j) = 100;
    }
  }
  
  for(int i = 0; i < n; i++){
    M(v1[i]-1,v2[i]-1) = dis[i];
  }
  
  return(M);
}

// [[Rcpp::export]]

int nearest_neighbor(NumericMatrix dist_matrix, int current_city, NumericVector untraveled_cities){
  int nn = 0;
  double min_dis = std::numeric_limits<float>::infinity();
  for(int i = 0; i < untraveled_cities.size(); i++){
    if((dist_matrix(current_city-1, untraveled_cities[i]-1)) < min_dis){
      min_dis = dist_matrix(current_city-1, untraveled_cities[i]-1);
      nn = untraveled_cities[i];
    };    
  };
  
  return(nn);
};

// [[Rcpp::export]]

int nearest_neighbor_reverse(NumericMatrix dist_matrix, int current_city, NumericVector untraveled_cities){
  int nn = 0;
  double min_dis = std::numeric_limits<float>::infinity();
  for(int i = 0; i < untraveled_cities.size(); i++){
    if((dist_matrix(untraveled_cities[i]-1,current_city-1) < min_dis)){
      min_dis = dist_matrix(untraveled_cities[i]-1, current_city-1);
      nn = untraveled_cities[i];
    };    
  };
  
  return(nn);
};

// Nearest Insertion

// [[Rcpp::export]]
int nearest_insertion(NumericMatrix dist_matrix, NumericVector current_tour){
  int nn = 0;
  int nearest_node = 0;
  double min_dis = std::numeric_limits<float>::infinity();
  int n = dist_matrix.rows();
  NumericVector s(n);
  NumericVector untraveled_cities;
  
  s = seq(1, n);
  for(int i = 0; i < s.size(); i++){
    int key = s[i];
    if (!std::count(current_tour.begin(), current_tour.end(), key)) {
      untraveled_cities.push_back(key);
    }
  }
  
  
  for(int i = 0; i < current_tour.size(); i++){
    nn = nearest_neighbor(dist_matrix,current_tour[i],untraveled_cities);
    
    if((dist_matrix(current_tour[i]-1, nn-1)) < min_dis){
      nearest_node = nn;
      min_dis = dist_matrix(current_tour[i]-1, nn-1);
    }; 
  };
  
  return(nearest_node);
};


// [[Rcpp::export]]
// From sequence to matrix representation of a tour

NumericMatrix matrix_tour(NumericVector sequence){
  NumericMatrix X(sequence.size()-1);
  for(int i = 0; i < sequence.size()-1; i++){
     X(sequence[i]-1,sequence[i+1]-1) = 1; 
  };
  return(X);
};

// [[Rcpp::export]]

// Swaping edges that start in positions i, k of a given tour, to get a new tour
NumericVector TwoOptSwap( NumericVector tour, const int& i, const int& k )
{
  int n = tour.length();
  NumericVector new_tour(n);
  
  // 1. take route[0] to route[i-1] and add them in order to new_route
  for ( int c = 0; c < i; ++c ) {
    new_tour[c] = tour[c];  }
  
  // 2. take route[i] to route[k] and add them in reverse order to new_route
  int dec = 0;
  for ( int c = i; c < k; ++c )
  { new_tour[c] = tour[k - 1 - dec];
    dec++;}
  
  // 3. take route[k+1] to end and add them in order to new_route
  for ( int c = k ; c < n; ++c )
  { new_tour[c] = tour[c];}
  return (new_tour);
};




// Swaping edges that start in positions i, k of a given tour, to get a new tour
NumericVector ThreeOptSwap_1( NumericVector tour, const int& i, const int& j, const int& k )
{
  int n = tour.length();
  NumericVector new_tour(n);
  
  // 1. take route[0] to route[i-1] and add them in order to new_route
  for ( int c = 0; c < i; ++c ) {new_tour[c] = tour[c];}
  
  // 2. take route[i] to route[j-1] and add them in reverse order to new_route
  int dec = 0;
  for ( int c = i; c < j; ++c )  {new_tour[c] = tour[j - 1 - dec]; dec++;
  }
  
  // 3. take route[j] to route[k-1] and add them in reverse order to new_route
  int dec1 = 0;
  for ( int c = j; c < k; ++c )
  {  new_tour[c] = tour[k - 1 - dec1];
    dec1++;}
  
  // 3. take route[k] to end and add them in order to new_route
  for ( int c = k ; c < n; ++c )
  {   new_tour[c] = tour[c];}
  return (new_tour);
};



// Swaping edges that start in positions i,j,k of a given tour, to get a new tour
NumericVector ThreeOptSwap_2( NumericVector tour, const int& i, const int& j, const int& k )
{
  int n = tour.length();
  NumericVector new_tour(n);
  
  // 1. take route[0] to route[i-1] and add them in order to new_route
  for ( int c = 0; c < i; ++c ) {
    new_tour[c] = tour[c];}
  
  // 2. take route[j] to route[k-1] and add them after i-1
   
  for( int c = 0; c < (k-j); ++c )  {
     new_tour[i+c] =   tour[j+c];
  }
  
  // 3. take route[i] to route[j-1] and add them after k-1
  
  for ( int c = 0; c < (j-i); ++c )
  {  new_tour[i+k-j+c] = tour[i+c];
    }
  
  // 3. take route[k] to end and add them in order to new_route
  for ( int c = k ; c < n; ++c )
  {   new_tour[c] = tour[c];}
  return (new_tour);
};


// Swaping edges that start in positions i, j, k of a given tour, to get a new tour
NumericVector ThreeOptSwap_3( NumericVector tour, const int& i, const int& j, const int& k )
{
  int n = tour.length();
  NumericVector new_tour(n);
  
  // 1. take route[0] to route[i-1] and add them in order to new_route
  for ( int c = 0; c < i; ++c ) {new_tour[c] = tour[c];}
  
  // 2. take route[j] to route[k-1] and add them after i-1
  
  for( int c = 0; c < (k-j); ++c )  {
    new_tour[i+c] =   tour[j+c];
  }
  
  // 3. take route[i] to route[j-1] and add them after k-1 in reverse order
  
  for ( int c = 0; c < (j-i); ++c )
  { new_tour[i+k-j+c] = tour[j-1-c];
  }
  
  // 3. take route[k] to end and add them in order to new_route
  for ( int c = k ; c < n; ++c )
  {   new_tour[c] = tour[c];}
  return (new_tour);
};


// Swaping edges that start in positions i,j,k of a given tour, to get a new tour
NumericVector ThreeOptSwap_4( NumericVector tour, const int& i, const int& j, const int& k )
{
  int n = tour.length();
  NumericVector new_tour(n);
  
  // 1. take route[0] to route[i-1] and add them in order to new_route
  for ( int c = 0; c < i; ++c ) {
    new_tour[c] = tour[c];}
  
  // 2. take route[j] to route[k-1] and add them after i-1 in reverse order
  int dec1 = 0;
  
  for( int c = 0; c < (k-j); ++c )  {
    new_tour[i+c] =   tour[k-1-dec1];
    dec1++;
  }
  
  // 3. take route[i] to route[j-1] and add them after k-1
  
  for ( int c = 0; c < (j-i); ++c )
  {  new_tour[i+k-j+c] = tour[i+c];
  }
  
  // 3. take route[k] to end and add them in order to new_route
  for ( int c = k ; c < n; ++c )
  {   new_tour[c] = tour[c];}
  return (new_tour);
};

// [[Rcpp::export]]
NumericVector ThreeOptSwap( int option, NumericVector tour, const int& i, const int& j, const int& k ) {
  int n = tour.length();
  NumericVector new_tour(n);
  
  switch(option) {
  case 1:
    new_tour = ThreeOptSwap_1(tour, i, j, k );
    break;
  case 2:
    new_tour = ThreeOptSwap_2(tour, i, j, k );
    break;
  case 3:
    new_tour = ThreeOptSwap_3(tour, i, j, k );
    break;
  case 4:
    new_tour = ThreeOptSwap_4(tour, i, j, k );
    break;
  default:
    new_tour = tour;
  }

  return(new_tour);
};
// [[Rcpp::export]]
// To get the distance of a tour

double tour_length( NumericMatrix dist_matrix, NumericVector tour){
  if(tour.length() == 0) { return(0);} else{
    int n = tour.length();
    double distance = 0;
    for(int i = 0; i < n-1; ++i) {
      distance += dist_matrix(tour[i]-1,tour[i+1]-1);
    }
    return(distance);
  }
  
}

// [[Rcpp::export]]
NumericVector cumsumF( NumericMatrix dist_matrix, NumericVector tour){
  if(tour.length() == 0) { return(0);} else{
    int n = tour.length();
    NumericVector F (n-1);
    F[0] = dist_matrix(tour[0]-1,tour[1]-1);
    for(int i = 1; i < (n-1); ++i) {
      F[i] = F[i-1] + dist_matrix(tour[i]-1,tour[i+1]-1);
    }
    return(F);
  }
}

// [[Rcpp::export]]
NumericVector cumsumB( NumericMatrix dist_matrix, NumericVector tour){
  if(tour.length() == 0) { return(0);} else{
    int n = tour.length();
    NumericVector B (n-1);
    B[n-2] = dist_matrix(tour[n-1]-1,tour[n-2]-1);
    for(int i = 1; i < (n-1); ++i) {
      B[n-2-i] = B[n-2-i+1] + dist_matrix(tour[n-1-i]-1,tour[n-2-i]-1);
    }
    return(B);
  }
}


// Function for the 2-opt improvement algorithm, given an edge1 (pos1, pos2) finds the other edge2,
// reconnect them and get a tour with a smaller distance (if possible), otherwise returns the 
// same tour
// i, k are index, the edges to swap are (i,i+1) and (k,k+1). These index start from 1.


double gain_2opt (NumericVector tour, NumericMatrix C, int i, int k){
  int n_i = tour(i-1)-1;
  int n_i_1 = tour(i)-1;
  int n_k = tour(k-1)-1;
  int n_k_1 = tour(k)-1;
  double gain = -C(n_i,n_k) - C(n_i_1,n_k_1)+C(n_i,n_i_1)+C(n_k,n_k_1);
  return(gain);
}


double gain_2opt_A (NumericVector tour, NumericMatrix C, int i, int k, NumericVector F, NumericVector B){
  int n_i = tour(i-1)-1;
  int n_i_1 = tour(i)-1;
  int n_k = tour(k-1)-1;
  int n_k_1 = tour(k)-1;
  double B_dif = B[i] - B[k-1];
  double F_dif = F[k-2] - F[i-1];
  double gain = -C(n_i,n_k) - C(n_i_1,n_k_1)+C(n_i,n_i_1)+C(n_k,n_k_1)-B_dif+F_dif;
  return(gain);
}

// Function for computing the gain in one of the different 3-opt swaps 
// i, j, k are index, the edges to swap are (i,i+1), (j,j+1) and (k,k+1). These index start from 1.


// [[Rcpp::export]]
double gain_3opt_A (int option, NumericVector tour, NumericMatrix C, int i, int j, int k, NumericVector F, NumericVector B){
  int n_i = tour(i-1)-1; // i
  int n_i_1 = tour(i)-1; // i+1
  int n_j = tour(j-1)-1; // j
  int n_j_1 = tour(j)-1; // j+1
  int n_k = tour(k-1)-1; // k
  int n_k_1 = tour(k)-1; // k+1
  double gain = 0;
  double B_dif = 0;
  double F_dif = 0;
  
  switch(option) {
  case 1:
    B_dif = B[i] - B[j-1] + B[j] - B[k-1];
    F_dif = F[k-2] - F[j-1] + F[j-2] - F[i-1];
    gain = -C(n_i,n_j) - C(n_i_1,n_k) - C(n_j_1,n_k_1) +C(n_i,n_i_1) +C(n_j,n_j_1) +C(n_k,n_k_1) -B_dif+F_dif;
    break;
  case 2:
    gain = -C(n_i,n_j_1) - C(n_j,n_k_1) - C(n_k,n_i_1) +C(n_i,n_i_1) +C(n_j,n_j_1) +C(n_k,n_k_1);
    break;
  case 3:
    B_dif = B[i] - B[j-1] ;
    F_dif = F[j-2] - F[i-1];
    gain = -C(n_i,n_j_1) - C(n_i_1,n_k_1) - C(n_k,n_j) +C(n_i,n_i_1) +C(n_j,n_j_1) +C(n_k,n_k_1) -B_dif+F_dif;
    break;
  case 4:
    B_dif =  B[j] - B[k-1];
    F_dif = F[k-2] - F[j-1];
    gain = -C(n_i,n_k) - C(n_j_1,n_i_1) - C(n_j,n_k_1) +C(n_i,n_i_1) +C(n_j,n_j_1) +C(n_k,n_k_1) -B_dif+F_dif;
    break;
  default:
    gain = 0;
  }

  return(gain);
}

// [[Rcpp::export]]
NumericVector TwoOpt_New_Tour (NumericVector neighborhood_edge1, NumericVector current_tour, NumericMatrix dis_matrix, int min_distance){
  int n = current_tour.length();
  int pos1_1 = neighborhood_edge1[0];
  int pos1_2 = neighborhood_edge1[1];
  int pos2_1 = neighborhood_edge1[2];
  int pos2_2 = neighborhood_edge1[3];
  int previous_node_pos = neighborhood_edge1[4];
  int next_node_pos = neighborhood_edge1[5];
  
  for(int it = 1; it < (n); ++it) {
    
    // avoid adjacent edges
    if((it != pos1_1) && (it != pos1_2) && (it != pos2_1) && (it != pos2_2) && (it != previous_node_pos) && (it != next_node_pos) ){
      NumericVector pos1 = {pos1_1,pos1_2};
      int pos1_min = min(pos1);
      NumericVector v = {it, pos1_min};
      
      int i = min(v);
      int k = max(v);
      
      double gain = gain_2opt(current_tour, dis_matrix, i, k);
      
      if( gain > 0) {   // improvement found
        
        current_tour =  TwoOptSwap(current_tour, i, k); // update tour
        current_tour.push_back(gain); // store gain at the end of the tour, to keep track of it
        
        break;
      }
    }
  }
  return(current_tour);
}


// [[Rcpp::export]]
NumericVector TwoOpt_New_Tour_A (NumericVector neighborhood_edge1, NumericVector current_tour, NumericMatrix dis_matrix, int min_distance, NumericVector F, NumericVector B){
  int n = current_tour.length();
  int pos1_1 = neighborhood_edge1[0];
  int pos1_2 = neighborhood_edge1[1];
  int pos2_1 = neighborhood_edge1[2];
  int pos2_2 = neighborhood_edge1[3];
  int previous_node_pos = neighborhood_edge1[4];
  int next_node_pos = neighborhood_edge1[5];
  
  for(int it = 1; it < (n); ++it) {
    
    // avoid adjacent edges
    if((it != pos1_1) && (it != pos1_2) && (it != pos2_1) && (it != pos2_2) && (it != previous_node_pos) && (it != next_node_pos) ){
      NumericVector pos1 = {pos1_1,pos1_2};
      int pos1_min = min(pos1);
      NumericVector v = {it, pos1_min};
      
      int i = min(v);
      int k = max(v);
      
      double gain = gain_2opt_A(current_tour, dis_matrix, i, k, F, B);
      
      if( gain > 0) {   // improvement found
        
        current_tour =  TwoOptSwap(current_tour, i, k); // update tour  wed 2:15
        current_tour.push_back(gain); // store gain at the end of the tour, to keep track of it
        
        break;
      }
    }
  }
  return(current_tour);
}


// [[Rcpp::export]]
NumericVector TwoOpt_New_Tour_A_v2 (NumericVector neighborhood_edge1, NumericVector current_tour, NumericMatrix dis_matrix, int min_distance, NumericVector F, NumericVector B){
  int n = current_tour.length();
  int pos1_1 = neighborhood_edge1[0];
  int pos1_2 = neighborhood_edge1[1];
  int pos2_1 = neighborhood_edge1[2];
  int pos2_2 = neighborhood_edge1[3];
  int previous_node_pos = neighborhood_edge1[4];
  int next_node_pos = neighborhood_edge1[5];
  double max_gain = 0;
  int max_gain_i = 0;
  int max_gain_k = 0;
  
  for(int it = 1; it < (n); ++it) {
    
    // avoid adjacent edges
    if((it != pos1_1) && (it != pos1_2) && (it != pos2_1) && (it != pos2_2) && (it != previous_node_pos) && (it != next_node_pos) ){
      NumericVector pos1 = {pos1_1,pos1_2};
      int pos1_min = min(pos1);
      NumericVector v = {it, pos1_min};
      
      int i = min(v);
      int k = max(v);
      
      double gain = gain_2opt_A(current_tour, dis_matrix, i, k, F, B);
    
      
      if( gain > max_gain) {   // improvement found
        
        max_gain = gain;
        max_gain_i = i;
        max_gain_k = k;
        
      }
    }
  }
  if (max_gain > 0) { //max improvement
    current_tour =  TwoOptSwap(current_tour, max_gain_i, max_gain_k); // update tour
    current_tour.push_back(max_gain); // store gain at the end of the tour, to keep track of it
  }
  
  return(current_tour);
}


// 3-opt algorithm main function
// [[Rcpp::export]]
NumericVector ThreeOpt_BestSwap (NumericVector current_tour, NumericMatrix dis_matrix){
  
  int n = current_tour.length();
  double max_gain = 0;
  int max_gain_i = 0;
  int max_gain_j = 0;
  int max_gain_k = 0;
  int max_option = 0;
  NumericVector F = cumsumF(dis_matrix,current_tour);
  NumericVector B = cumsumB(dis_matrix,current_tour);
  NumericVector new_tour;
  int i = 1;

  while( i < (n-3)) {
      if(dis_matrix(current_tour[i-1]-1, current_tour[i]-1) == 0) {i++;}
      int j = (i+1);
    
      while( j <(n-2)){
        if(dis_matrix(current_tour[j-1]-1, current_tour[j]-1) == 0) {j++;}
        int k = j+1;
        
        while(k <(n-1)){
          if(dis_matrix(current_tour[k-1]-1, current_tour[k]-1) == 0) {k++;}
          
          for( int option = 1; option < 5; ++option) {
          
            double gain = gain_3opt_A(option, current_tour, dis_matrix, i, j, k, F, B);
            
            if( gain > max_gain) {   // improvement found
              
              max_gain = gain;
              max_gain_i = i;
              max_gain_j = j;
              max_gain_k = k;
              max_option = option;
              }
            
          } //for
              
         k++;   
            
       } //while3 k 
        
        j++; 
        
      }  //while2 j
    
        i++;
    
    } //while1 i
     
    if(max_gain > 0){ 
      new_tour = ThreeOptSwap(max_option, current_tour, max_gain_i, max_gain_j, max_gain_k);
      
    }
    else {
      new_tour = current_tour;
    }
    
      new_tour.push_back(max_gain); // store gain at the end of the tour, to keep track of it
  return(new_tour);
}

// [[Rcpp::export]]
NumericVector ThreeOpt_BestSwap_start (NumericVector current_tour, NumericMatrix dis_matrix){
  
  int n = current_tour.length();
  double max_gain = 0;
  int max_gain_i = 0;
  int max_gain_j = 0;
  int max_gain_k = 0;
  int max_option = 0;
  NumericVector F = cumsumF(dis_matrix,current_tour);
  NumericVector B = cumsumB(dis_matrix,current_tour);
  NumericVector new_tour;
  int i = 1;
  
    int j = (i+1);
    
    while( j <(n-2)){
      if(dis_matrix(current_tour[j-1]-1, current_tour[j]-1) == 0) {j++;}
      int k = j+1;
      
      while(k <(n-1)){
        if(dis_matrix(current_tour[k-1]-1, current_tour[k]-1) == 0) {k++;}
        
        for( int option = 1; option < 5; ++option) {
          
          double gain = gain_3opt_A(option, current_tour, dis_matrix, i, j, k, F, B);
          
          if( gain > max_gain) {   // improvement found
            
            max_gain = gain;
            max_gain_i = i;
            max_gain_j = j;
            max_gain_k = k;
            max_option = option;
          }
          
        } //for
        
        k++;   
        
      } //while3 k 
      
      j++; 
      
    }  //while2 j
    
  
  if(max_gain > 0){ 
    new_tour = ThreeOptSwap(max_option, current_tour, max_gain_i, max_gain_j, max_gain_k);
    
  }
  else {
    new_tour = current_tour;
  }
  
  new_tour.push_back(max_gain); // store gain at the end of the tour, to keep track of it
  return(new_tour);
}



// [[Rcpp::export]]
NumericVector ThreeOpt_GreedySwap (NumericVector current_tour, NumericMatrix dis_matrix, double min_max_gain){
  
  int n = current_tour.length();
  double max_gain = min_max_gain;
  int max_gain_i = 0;
  int max_gain_j = 0;
  int max_gain_k = 0;
  int max_option = 0;
  NumericVector F = cumsumF(dis_matrix,current_tour);
  NumericVector B = cumsumB(dis_matrix,current_tour);
  NumericVector new_tour;
  bool continuar = true;
  int i = 1;
  
  while( i < (n-3) and continuar) {
    int j = (i+1);
    
    while( j <(n-2) and continuar){
      int k = j+1;
      
      while(k <(n-1) and continuar){
        
        
        for( int option = 1; option < 5; ++option) {
          
          double gain = gain_3opt_A(option, current_tour, dis_matrix, i, j, k, F, B);
          
          if( gain > max_gain) {   // improvement found
            
            max_gain = gain;
            max_gain_i = i;
            max_gain_j = j;
            max_gain_k = k;
            max_option = option;
            continuar = false;
          }
          
        } //for
        
        k++;   
        
      } //while3 k 
      
      j++; 
      
    }  //while2 j
    
    i++;
    
  } //while1 i
  
  if(max_gain > 0){ 
    new_tour = ThreeOptSwap(max_option, current_tour, max_gain_i, max_gain_j, max_gain_k);
    
  }
  else {
    new_tour = current_tour;
  }
  
  new_tour.push_back(max_gain); // store gain at the end of the tour, to keep track of it
  return(new_tour);
}


// Classic split algorithm
// giant tour, sequence of customers visited in the tour omitting he depot
// [[Rcpp::export]]

NumericVector split (NumericVector giant_tour, double Q, NumericVector demand, NumericMatrix M) {
  int n = giant_tour.length()-1;
  NumericVector p(n+1); // cost of the shortest path from depot to customer n
  NumericVector pred(n+1); //predecessor of customer nth
  double load, cost;
  
  p[0] = 0;
  pred[0]= -1;
  for (int k = 1; k < (n+1); ++k) {
    p[k] = std::numeric_limits<float>::infinity();
    pred[k] = -1;
  }
  for (int t = 0; t < (n); ++t){
    
    int i = t+1;
    load = 0;
    
    while( (i <= (n)) and ((load + demand[giant_tour[i]-1]) <= Q) ){
      load += demand[giant_tour[i]-1];
      if(i == (t+1)){
        cost = M(0,giant_tour[i]-1);
      } else{
        cost += M(giant_tour[i-1]-1,giant_tour[i]-1);
      }
      if ((p[t] + cost + M(giant_tour[i]-1,0)) < p[i]) {
        p[i] = p[t] + cost + M(giant_tour[i]-1,0);
        pred[i] = t;
      }
      i += 1;
    }
  }
  pred.push_back(p[n]);
  
  return(pred);
}

// [[Rcpp::export]]

NumericVector split_replenish (NumericVector giant_tour, double Q, NumericVector demand, NumericMatrix M, int dumpsite) {
  int n = giant_tour.length()-1; // n number of customers
  NumericVector p(n+1); // cost of the shortest path from depot to customer n
  NumericVector pred(n+1); //predecessor of customer nth
  double load, cost;
  bool from_depot = TRUE;
  
  p[0] = 0;
  pred[0]= -1;
  for (int k = 1; k < (n+1); ++k) {
    p[k] = std::numeric_limits<float>::infinity();
    pred[k] = -1;
  }
  
  for (int t = 0; t < (n); ++t){
    
    int i = t+1;
    load = 0;
    if( (t == 0) ) {from_depot = TRUE;} else {from_depot = FALSE;}
    
    while( (i <= (n)) and ((load + demand[giant_tour[i]-1]) <= Q) ){
      load += demand[giant_tour[i]-1];
      if(i == (t+1)){
        if(!from_depot) {
          cost = M(dumpsite-1,giant_tour[i]-1);} else {
            cost = M(0,giant_tour[i]-1);}
      } else{
        cost += M(giant_tour[i-1]-1,giant_tour[i]-1);
      }
      
      
      if(i < n) {
        if ((p[t] + cost + M(giant_tour[i]-1,dumpsite-1)) < p[i]) {
          p[i] = p[t] + cost + M(giant_tour[i]-1,dumpsite-1);
          pred[i] = t;
        }
      } else{
        
        if ((p[t] + cost + M(giant_tour[i]-1,dumpsite-1) + M(dumpsite-1,0)) < p[i]) {
          p[i] = p[t] + cost + M(giant_tour[i]-1,dumpsite-1) + M(dumpsite-1,0);
          pred[i] = t;  
        }
      }
      i += 1;
    }
  }
  
  pred.push_back(p[n]);
  
  return(pred);
}

// [[Rcpp::export]]
List split_replenish_time (NumericVector giant_tour, double Q, NumericVector demand, NumericMatrix M, int dumpsite, NumericVector collection_time, NumericMatrix travel_time) {
  int n = giant_tour.length()-1; // n number of customers
  NumericVector p(n+1); // cost of the shortest path from depot to customer n
  NumericVector pred(n+1); //predecessor of customer nth
  NumericVector tm(n+1); // el tiempo que toma hacer la ruta hasta el node k siguiendo el shortest path
  double load, cost, time;
  bool from_depot = TRUE;
  
  p[0] = 0;
  pred[0]= -1;
  tm[0];
  for (int k = 1; k < (n+1); ++k) {
    p[k] = std::numeric_limits<float>::infinity();
    pred[k] = -1;
    tm[k] = 0;
  }
  
  for (int t = 0; t < (n); ++t){
    
    int i = t+1;
    load = 0;
    if( (t == 0) ) {from_depot = TRUE;} else {from_depot = FALSE;}
    
    while( (i <= (n)) and ((load + demand[giant_tour[i]-1]) <= Q) ){
      load += demand[giant_tour[i]-1];
      if(i == (t+1)){
        if(!from_depot) {
        cost = M(dumpsite-1,giant_tour[i]-1);
        time = travel_time(dumpsite-1,giant_tour[i]-1) + collection_time[giant_tour[i]-1];
          } else {
        cost = M(0,giant_tour[i]-1);
        time = travel_time(0,giant_tour[i]-1) + collection_time[giant_tour[i]-1];
            }
      } else{
        cost += M(giant_tour[i-1]-1,giant_tour[i]-1);
        time += travel_time(giant_tour[i-1]-1,giant_tour[i]-1) + collection_time[giant_tour[i]-1];
      }
      
      // todo termina en el dumpsite, cuando recuperemos la solucion hay q sumarle el costo de dumpsite-depot
      
        if ((p[t] + cost + M(giant_tour[i]-1,dumpsite-1)) < p[i]) {
          p[i] = p[t] + cost + M(giant_tour[i]-1,dumpsite-1);
          pred[i] = t;
          tm[i] = tm[t] + time + travel_time(giant_tour[i]-1,dumpsite-1) + collection_time[dumpsite-1];
        }
      
      i += 1;
    }
  }
  
  //pred.push_back(p[n]);
  
  List L = List::create(pred, tm, p);
  
  return(L);
  
  //return(pred);
}

