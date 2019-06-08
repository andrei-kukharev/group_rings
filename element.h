/*
The class realizes a element of the group ring (group algebra) R=FG, where G is a group, F is a finite field.
*/

#ifndef __ELEMENT_H_
#define __ELEMENT_H_

#include "groupring.h"

using namespace std;

class GroupRing;

class Element {
 private:
   int *cf;   // array of coefficients
   int n;     // =|G| (the order of the group)
   int p;     // = char F (the characteristic of the field)
   int num;   // size of the the ring (it is assumed that e=1*g1)
   const GroupRing* pR;  // the pointer to the base ring    
 private:
	Element() { };     							// default constructor, closed
 public:
   Element(const GroupRing* ); 				// use this constructor
   Element(const Element&);                // copy constructor
   ~Element();                             // destructor
   Element& operator = (const Element&);  // assignment operator (b=a)

	// Reload operators:
   friend Element operator * (const Element& a, const Element& b);   // z = x * y;
   friend Element operator + (const Element& a, const Element& b);   // z = x + y;
   friend Element operator - (const Element& a, const Element& b);   // z = x - y;
   friend bool operator == (const Element&, const Element&);         // test x==y; 
   
   // Filling the coefficients of the element:
   void Zero();     // zero element (num=0)
   void One();      // identity element (num=1)
   void Norm();     // reduction in module p
   void Num(int m); // make the element number in the ring = m (from 0)
   void Next();     // increment the number of the element
   void Rand();     // generates a random element of the ring

   // Input-output:
   friend ostream &operator << (ostream &stream, const Element&);  // output
   void ShowList() const; // output the element as a list of the coefficients
   int LoadFromFile(const char *filename); // load the list of coef. from file 
	int ReadCoefFromStream(ifstream &fin);  // read coef. from stream fin
	int SetCoefByStr(const char *str);  // input list of coef.

	const GroupRing* GetPointerRing() const {return pR;}	//return pointer to the ring
	
};

//---------
#endif

