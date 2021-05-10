#include<iostream>
#include<cmath>

using namespace std;

double fun(double x1, double x2) {
    return pow(x1,2) + 4*pow(x2,2) + sin(x1-x2);
}

double dif1(double x1,double x2) {
    return 2*x1 + cos(x1-x2);
}

double dif2(double x1,double x2) {
    return 8*x2 - cos(x1-x2);
}

double 𝛆 = 1e-5;

double min ( double z1, double z2, double h1, double h2){
    double A = 0, B = 0, s = 1, l = (1 + sqrt(5.0)) / 2;
    double FA = fun(z1, z2);
    double FH = fun(z1 + s*h1, z2 + s*h2);
    
    while(FA > FH) {
        s = 2*s;
        FH = fun(z1 + s*h1, z2 + s*h2);
    }
  
    B = s;
    double d = (B-A)/pow(l,2);
    double x = A+d;
    double y = B-d;
    double FX = fun(z1+x*h1, z2+x*h2);
    double FY = fun(z1+y*h1, z2+y*h2);
    while (B-A > 2*𝛆) {
        if(FX>FY) { A=x;x=y; FX=FY;y=A+B-x; FY=fun(z1+y*h1,z2+y*h2); }
    else
    { B=y;y=x;FY=FX;x=A+B-y;FX=fun(z1+x*h1,z2+x*h2); }
  }
  
  return (A+B)/2;
}


int main() {
    int i = 0;
    double z1 = 1, z2 = 1, g1 = 0, g2 = 0, N = 0, beta = 0;
    double h1 = -dif1(z1,z2), h2 = -dif2(z1,z2), smin = 0;
    double n = sqrt(pow(h1, 2) + pow(h2, 2));

    while(n > 𝛆) {
        smin = min (z1, z2, h1, h2);
        
        z1 = z1 + smin*h1;
        z2 = z2 + smin*h2;
        
        g1 = dif1(z1,z2);
        g2 = dif2(z1,z2);
        
        N = n;
        n = sqrt(pow(g1,2) + pow(g2,2));
        beta = pow(n,2) / pow(N,2);
        
        h1 = -g1 + beta*h1;
        h2 = -g2 + beta*h2;
        i++;
        
    }
    

    double z12 = 1, z22 = 1, g12 = 0, g22 = 0, N2 = 0, beta2 = 0;
    double h12 = -dif1(z12,z22), h22 = -dif2(z12,z22), smin2 = 0;
    double n2 = sqrt(pow(h12,2) + pow(h22,2));
    int j = 0;
    
    while(n2 > 𝛆) {
        smin2 = min(z12,z22,h12,h22);
        
        z12 = z12 + smin2*h12;
        z22 = z22 + smin2*h22;
        
        g12 = dif1(z12,z22);
        g22 = dif2(z12,z22);
        
        N2=n2;
        n2=sqrt(pow(g12,2)+pow(g22,2));
        
        if(j%2 == 0)
            beta2 = 0;
        else
            beta2 = pow(n2,2)/pow(N2,2);
        
        h12 = -g12 + beta2*h12;
        h22 = -g22 + beta2*h22;
        j++;
        
    }
    
    cout<<"Before obnulenij: "<< endl<< endl;
    cout<<"MIN = "<<"("<<z1<<", "<<z2<<")"<< endl;
    cout<<"Fmin = "<< fun (z1,z2) << endl;
    cout<<endl;
    cout<<"number of iter "<<i<<endl;
    cout<<"z = "<<"("<< z1 <<", " << z2 <<")"<<endl;
    cout<<"f(z) = "<< fun (z1,z2) <<endl;
    cout<<"|f'(z)|="<<sqrt(pow(dif1(z1,z2),2) + pow(dif2(z1,z2),2))<<endl;
    cout<<endl;
    cout<<"After obnulenij: "<<endl<<endl;
    cout<<"MIN = "<<"("<< z12 <<", "<< z22 <<")"<<endl;
    cout<<"Fmin = "<< fun (z12,z22) <<endl;
    cout<<endl;
    cout<<"number of iter  "<<j<<endl;
    cout<<"z="<<"("<< z12 <<","<< z22 <<")"<<endl;
    cout<<"f(z) = "<< fun (z12,z22) <<endl;
    cout<<"|f'(z)|="<<sqrt(pow(dif1(z12,z22),2) + pow(dif2(z12,z22),2))<<endl;

    return 0;
}

