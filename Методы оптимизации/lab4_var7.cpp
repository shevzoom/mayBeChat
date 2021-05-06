#include <iostream>
#include <math.h>
#include <conio.h>
using namespace std;

const double eps = 0.00001, epss = 0.001;

double f(double x1, double x2, double x3)
{
	return x1*x1+(x3-1)*(x3-1)+2*x2-x1; //функция
}

double f1(double x1, double x2, double x3) 
{
	return (x1-4)*(x1-4)+x2*x2+x3*x3-4;           //в методе это ф итое
}


double H(double x1, double x2, double x3) // в методе это H
{ 
	double h=0;
	if (f1(x1, x2, x3)>=0) 
		h+=pow(f1(x1, x2, x3),2);
	return h;
}


double h1(double x1, double x2, double x3, int a)   //производная по нашей получившейся fn
{
	double h, res;
	
		res=2*x1-1;
	if (f1(x1, x2,x3)>=0) 
		res+=2*((x1-4)*(x1-4)+x2*x2+x3*x3-4)*2*(x1-4)*a;
	
	return res;
}
double h2(double x1, double x2, double x3, int a)
{
	double res;
			
		res=2;
	if (f1(x1, x2,x3)>=0) 
		res=2*((x1-4)*(x1-4)+x2*x2+x3*x3-4)*2*x2*a;
	return res;
}

double h3(double x1, double x2, double x3, int a)
{
	double h, df1, df2, res;
			
		res=2*(x3-1);
	if (f1(x1, x2,x3)>=0) 
		res=2*((x1-4)*(x1-4)+x2*x2+x3*x3-4)*2*x3*a;
	return res;
}


double fn(double x1, double x2,double x3, double n) //fn в методе написано
{
	return (f(x1, x2, x3) + n*H(x1, x2, x3));
}

void main()
{   
	double k=0;
	int K=0,a=k;
    	double x1=0.6, x2=1.2,x3=2.0,s=1,alf=0.2,sigma=0.2;
	double n=sqrt(h1(x1,x2,x3,a)*h1(x1,x2,x3,a)+h3(x1,x2,x3,a)*h3(x1,x2,x3,a)+h2(x1,x2,x3,a)*h2(x1,x2,x3,a));	
	cout<<n;
	getch();
fl:  
{  K++;
    a=10*(K+1);  //аn->oo  (нам надо любую такую последовательность взять
//f2:{s=1;
//f2:
	do
	{
		if((fn(x1,x2,x3,a)-fn(x1-s*h1(x1,x2,x3,a),x2-s*h2(x1,x2,x3,a),x3-s*h3(x1,x2,x3,a),a))<=(sigma*n*n*s*s))   // идёт метод 2.2 в методе ищем n=|h|
		{s=alf*s;
		x1=x1-s*h1(x1,x2,x3,a);   //всё ещё 2.2 идёт, меняем х как в методе
		x2=x2-s*h2(x1,x2,x3,a);
		x3=x3-s*h3(x1,x2,x3,a);}
		n=sqrt(h1(x1,x2,x3,a)*h1(x1,x2,x3,a)+h3(x1,x2,x3,a)*h3(x1,x2,x3,a)+h2(x1,x2,x3,a)*h2(x1,x2,x3,a));
		cout<<n;
   }while(n>=eps);
   //if (n>eps) { goto f2;}   //nj;t 2.2
   if (sqrt(H(x1,x2,x3))>=epss)  goto fl;} // а это уже 3.1 

   cout<<"nomer iteracii="<<K<<endl;
   cout<<"x=" <<"("<<x1<<","<<x2<<")"<<endl;
   cout<<"f(x)="<<f(x1,x2,x3)<<endl;
   cout<<"a="<<a<<endl;
   cout<<"H="<<sqrt(H(x1,x2,x3))<<endl;
   cout<<endl;
   cin>>k;
}
