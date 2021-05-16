#include <iostream>
#include <cmath>

using namespace std;

// epsilon
const double e2 = 0.001;

double f(double x1, double x2) {
//    return (pow(x1,2)+(x3-1)*(x3-1)+2*x2-x1);
    return pow(x1,2)+pow(x2,2)-10*x1-8*x2+3;
}

// H(X) is shtraf func == constraint area
double H(double x1,double x2) {
    double h = 0;
    double f1 = x1 + 2*x2 - 2;
    if (f1 > 0)
        h = h+ pow(f1, 2);
    return h;
}

double fn(double x1,double x2, double a) {
    return f(x1,x2) + a*H(x1,x2);
}

// ? z = z + 2*f^2 * f'^2
double gr1(double x1,double x2,double a) {
    double z = 2*x1 - 10;
    double f1 = x1 + 2*x2 - 2;
    
    if (f1 > 0) z = z + 2*pow(f1, 2)*2*a;
    return z;
}

double gr2(double x1,double x2,double a) {
    double z = 2*x2 - 8;
    double f1 = x1 + 2*x2 - 2;

    if (f1 > 0) z = z + 2*pow(f1, 2)*2*2*a ;
    return z;
}

void scorspusk(double* Z,double j) {
    double e = 0.000001, h1,h2, z1=0.1,z2=0.1, n;
    double l=(1+sqrt(5.0))/2, FX,FY, s=1,A=0,B=1,Smin=0;

    h1=-gr1(z1,z2,j);
    h2=-gr2(z1,z2,j);
       
    int i=0;
    while(sqrt(h1*h1 + h2*h2) > e) {
        i++;
        FX = f(z1,z2);
        double FH = f(z1+s*h1, z2+s*h2);
        while(FX > FH) {
            s*=2;
            FH=f(z1+s*h1, z2+s*h2);
        }
        B=s;
        double d = (B-A)/(l*l);
        double X = A+d, Y = B-d;
        double FX = f(z1+X*h1, z2+X*h2), FY = f(z1+Y*h1, z2+Y*h2);
        
        while((B-A) > 2*e) {
            FX = f(z1+X*h1, z2+X*h2); FY = f(z1+Y*h1, z2+Y*h2);
            if(FX>FY) {
                A=X;X=Y;FX=FY;Y=A+B-X;FY=f(z1+Y*h1, z2+Y*h2);
            } else {
                B=Y;Y=X;FY=FX;X=A+B-Y;FX=f(z1+X*h1, z2+X*h2);
            }
        }
        Smin=(A+B)/2;
        
        z1 += Smin*h1;
        z2 += Smin*h2;

        h1=-gr1(z1,z2,j);
        h2=-gr2(z1,z2,j);
        
        n=sqrt(h1*h1 + h2*h2);
        
        FX = f(z1, z2);
    }
    Z[0]=z1;
    Z[1]=z2;
}

int main() {
    double Z[2];

    double n=0, z1,z2;
    int i=0;
    double xprev1=2.5;
    double xprev2=2.1;
    while(sqrt(H(xprev1, xprev2)) > e2) {
        i++;
        n=n+1;
        z1 = xprev1;
        z2 = xprev2;
        scorspusk(&z1,z2);
        xprev1 = Z[0];
        xprev2 = Z[1];
        cout<<"i "<<i<<endl;
    }

    
    cout<<"Number of iterations: "<<n<<endl<<endl;

    cout<<"x = ("<<xprev1<<";"<<xprev2<<";"<<")"<<endl;

    cout<<"fn(x) = "<<fn(xprev1, xprev2, 5);

    return 0;
}
