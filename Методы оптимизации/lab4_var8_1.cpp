#include<conio.h>
#include<stdio.h>
#include<iostream>
#include<cmath>
using namespace std;

const double e = 0.0001;

double f(double x1, double x2)
{
	return pow(x1,2)+pow(x2,2)-10*x1-8*x2+3;
}

double gr1(double x1)
{	
	return 2*x1 - 10;
}
double gr2(double x2)
{
	return 2*x2 - 8;
}

double fn (double s,double z1,double z2,double y1,double y2)
{ 
	double zz1=z1+s*(y1-z1), zz2=z2+s*(y2-z2);
	return f(zz1,zz2);
}

int main()
{
	int i=0;
	double j=(1+sqrt(5.0))/2,d,a=0,b=1,A,B,x,y,fx,fy,gr;
	double y1,y2, x1=0.0,x2=0.0, s,zz,g;
	
	double zz1,zz2;
	
	double z1=0.0, z2=0.0;
	do  
	{  
		i++;
		gr=sqrt(pow(gr1(z1),2)+pow(gr2(z2),2));
		y1=4-2*gr1(z1)/gr;
		y2=-2*gr2(z2)/gr;
		x1=y1, x2=y2;
        	d=(b-a)/(j*j);
		A=a;
		B=b;
		x=A+d;
		y=B-d;
		fx=fn(x,z1,z2,y1,y2);
		fy=fn(y,z1,z2,y1,y2);
		while((B-A)>2*e)
			if (fx>fy) 
			{
				A=x; 
				x=y; 
				fx=fy; 
				y=A+B-x; 
				fy=fn(y,z1,z2,y1,y2);
			}
			else
			{
				B=y;
				y=x; 
				fy=fx; 
				x=A+B-y; 
				fx=fn(x,z1,z2,y1,y2);
			}
		s=(B+A)/2; 
		zz1=z1+s*(y1-z1);
		zz2=z2+s*(y2-z2);

        	zz=sqrt(pow((zz1-z1),2)+pow((zz2-z2),2));
		g=sqrt(pow(gr1(z1),2)+pow(gr2(z2),2));
		if(zz>=e && g>=e) 
		{ 
			z1=zz1; 
			z2=zz2;
		} 
	}
	while (zz>=e && g>=e); 
    cout<<"Kol-vo iteraciy: "<<i<<endl<<endl;
	cout<<"   zz= ("<<zz1<<" ; "<<zz2<<" ; "<<'}'<<endl;
	cout<<"   s= "<<s<<endl;
	cout<<"   F= "<<f(z1,z2)<<endl;
	return 0;
}
