#include <stdio.h>  
#include <math.h>  
  
#define NUMBERS 1000009  
#define max_pow 20          //max pow for: 2^pow < 1.000.000  
  
int ways[NUMBERS];          //ways num can be reached - DP array  
long part[NUMBERS];         //sum from ways[0] up to ways[i] (mod m of course)  
int winstreaks[max_pow];    //pre-calculated winstreak earnings  
int m = 1757867;  
  
  
void init_powers(){         //__init winstreak earnings  
    int pow_acc = 2;      
    for(int i = 1; i <= max_pow; i++) {  
        winstreaks[i-1] = pow_acc - 1;  //1,3,7,15,31,63,...  
        pow_acc *= 2;  
    }  
}  
  
void init_ways(){     
    long long res;  
    for(int i = 2; i < NUMBERS; i++) {   //init DP_array  
        res = 0;  
          
        //what winstreak can I do from i? any with earnings <= i.   
        //so i search in pre-calculated winstreak earnings  
        for(int j = 0; j < max_pow && winstreaks[j] <= i; j++)  
            res += (long long) ways[i - winstreaks[j]]; //one more sub-tree  
          
        ways[i] = (int) (res % (long long) m);  
    }  
}  
  
void init_part() {  
    long long acc = 0;  
    long temp;  
      
    for(int i = 0; i < NUMBERS; i++){  
        acc = ((long long) ways[i] + acc) % (long long) m;  //calculate long and mod  
        temp = (long) acc;  
        part[i] = temp;     //store  
    }  
}  
  
//https://stackoverflow.com/questions/11720656/modulo-operation-with-negative-numbers  
long modulo_Euclidean(long a, long b) {  
    long m = a % b;  
    if (m < 0) {  
        m = (b < 0) ? m - b : m + b;  
    }  
    return m;  
}  
  
int main()   
{  
    int q;  
    scanf("%d %d\n", &q, &m);  
      
    //__INIT__ pre-calculated powers  
    init_powers();  
      
    // __INIT__ DP_array  
    ways[0] = 1;  
    ways[1] = 1;  
    init_ways();  
  
    // __INIT__ partial sums  
    init_part();  
  
    //show results  
    long res;  
    int n1, n2;  
    for(int i = 0; i < q; i++) {  
        scanf("%d %d\n", &n1, &n2);  
  
        //times 2 cause it may start with an 'x'  
        if(n1 == 0)   
            res = (2 * part[n2]) % m - 1;  
              
        else {  
            long dif = (long) part[n2] - (long) part[n1-1];   
            long temp_res = 2 * dif;          
            long x = modulo_Euclidean(temp_res, m);   
              
            res = (long) x;  
        }  
        printf("%u\n", res);  
    }  
  
    return 0;  
}  
