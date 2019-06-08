#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <cassert> 
#include "groupring.h"
#include "element.h"

#define DEBUG 0

using namespace std;

// Constructors
Element::Element(const GroupRing* pRing) : pR(pRing)
{
	n = pR->GetN();
   p = pR->GetP();
   cf = new int[n];
   for(int i=0; i<n; i++) cf[i] = 0;
}

// Copy constructor
Element::Element(const Element &x) : n(x.n), p(x.p), pR(x.pR)
{
	cf = new int[n];
   for(int i=0; i<n; i++) {
  		cf[i] = x.cf[i];
   }
}

// Assignment operator (b=a)
Element& Element::operator = (const Element &x) 
{
	n = x.n;
   p = x.p;
   pR = x.pR;
   num = x.num;
	for(int i=0; i<n; i++) {
   	cf[i] = x.cf[i];
   }
   return (*this);
}

Element::~Element()
{
	if (n>0) delete[] cf;	
}

// zero element
void Element::Zero()
{
	num = 0;
	for(int i=0; i<n; i++)  cf[i] = 0;
}

// identity element
void Element::One()
{
	num = 1;
	cf[0] = 1;
	for(int i=1; i<n; i++)  cf[i] = 0;
}

// reduction in module p
void Element::Norm()
{
   int *pt = cf;
   for(int i=0; i<n; i++) {
   	*(pt++) %= p;
   }	
	// alternative code: for(int i=0; i<n; i++)  cf[i] %= p;      
}

// creation element which has number m in the ring (from the 0-th)
void Element::Num(int m)  
{
	if (m < 0) throw "Out of rang";
   int q = num = m;
	for(int i=0; i<n; i++) {
      int k = q%p;
		q = q/p;
      cf[i] = k;
   }
}

// increment the number of the element
void Element::Next()  
{
   num++;
	for(int i=0; i<n; i++) {
      if (++cf[i] == p)  cf[i] = 0;
      else break;
   }
}

// function generates a random element of the ring
void Element::Rand()  
{
	for(int i=0; i<n; i++) {
   	cf[i] = rand() % p;
   }
}

// Reload operators:

// Product of the ring elements: z = x*y
Element operator * (const Element &x, const Element &y)	   
{
	Element z(x.pR);
   z.Zero();  // zerofilling
	int **tab = (z.pR)->table;  // multiplication table
	int *px = x.cf;	
	for(int i=0; i<z.n; i++, px++) {
		int *p0 = *(tab++);	// pointer to the 1-st element of the i-th row
		int *py = y.cf;	
   	for(int j=0; j<z.n; j++) {
   		z.cf[ *(p0++) - 1] += (*px) * (*(py++));  
      }
   }      
   z.Norm(); // reduction in module p    
   return z;
}

// z = x+y;
Element operator + (const Element &x, const Element &y)	   
{
	Element z(x.pR);
	for(int i=0; i<z.n; i++) {
   	z.cf[i] = x.cf[i] + y.cf[i];   // sum of coefficients
   }
   z.Norm();
   return z;
}

// z = x-y;
Element operator - (const Element &x, const Element &y)	   
{
	Element z(x.pR);
	for(int i=0; i<z.n; i++) {
   	z.cf[i] = x.cf[i] - y.cf[i] + z.p;   
   }
   z.Norm();
   return z;
}

// a==b
bool operator == (const Element &x, const Element &y) 	 
{
	int *px = x.cf;
	int *py = y.cf;	
	for(int i=0; i<x.n; i++) {
		if ( *(px++) != *(py++) ) return false;  //alternative: if (x.cf[i]!=y.cf[i]) return false;		
   }
   return true;
}


// Output

ostream &operator << (ostream &stream, const Element &x)
{
	bool first = true;	
   
	switch( (x.GetPointerRing())->outputFormat ) {
		
		case 1: // as sum of gi

			for(int i = 0; i < x.n; i++) {
				if (x.cf[i] != 0) {
					if (first) first = false;
					else cout << " + ";
					stream << x.cf[i] << "*g" << (i+1);
				}
			}
		   stream << ";"; 
			break;
			
		case 2: // as sum of g[i]

			for(int i = 0; i < x.n; i++) {
				if (x.cf[i] != 0) {
					if (first) first = false;
					else cout << " + ";
					stream << x.cf[i] << "*g[" << (i+1) << "]";
				}
			}
		   stream << ";"; 
			break;			
      
		default: // as sequence of coefficients [1 0 0 1 0 ...]

		   stream << "[ "; 
			for(int i = 0; i < x.n; i++) {
					stream << x.cf[i] << " ";
			}
		   stream << "];"; 
			break;		
			
	}			
   return stream;
}

void Element::ShowList() const
{
	cout << "[";
	for(int i=0; i < n-1; i++) {
   	cout << cf[i] << ", ";
	}
	cout << cf[n-1] << "]";
}

// load the list of coefficients from file 
int Element::LoadFromFile(const char *filename)
{   
   ifstream fin(filename);
   if (!fin) throw "Error: File not found.";
   for(int i=0; i<n; i++) {
      if (fin.eof()) throw "Error: the end of the file";
      int x;
      fin >> x;
      cf[i] = x;
   }
   fin.close();
	return 0;
}

int Element::ReadCoefFromStream(ifstream &fin)
{   
   for(int i=0; i<n; i++) {
      if (fin.eof()) throw  "Error: the end of the file";
      int x;
      fin >> x;
      cf[i] = x;
   }
	return 0;
}

int Element::SetCoefByStr(const char *str)
{  
   std::stringstream ss;
	ss << str;
	
   for(int i=0; i<n; i++) {
		int x;
      ss >> x;
      cf[i] = x;
   }   
   
	return 0;	
}

