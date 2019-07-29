## Checking whether a group ring is serial/uniserial
## Language: GAP

Read("module_struct.txt");

IsomList := function(R, Le)
	local i,j,n,m,ei,ej,res,class;
	n:=Size(Le);
	class:=[];  ## equivalence classes
	for i in [1..n] do class[i]:=i; od;

	for i in [1..n-1] do
		for j in [i+1..n] do
			if class[i]=class[j] then continue; fi;			
			ei:=Le[i];
			ej:=Le[j];
			res:=Isom(R,ei,ej);
			Print("Isom. e",i,"*R and e",j,"*R - ",res,"\n");
			if res then class[j]:=class[i]; fi;
			Display(class);
		od;
	od;
	Print("\n");
	
	for i in [1..n] do
		m:=0;
		for j in [1..n] do
			if class[j] = i then
				if m=0 then
					Print(j);
				else
					Print("~",j);
				fi;
				m := m+1;
			fi;
		od;
		if m>0 then
			Print(", ");
		fi;
	od;
	Print("\n");

end;;

## Checking whether two R-modules are isomorphic.
## there is exist r in e1*R*e2, s in e2*R*e1, that r*s=e1 and s*r=e2
## returns 1 if they are isomorphic, and 0 overwise

Isom := function ( R, e1, e2 )
local debug,i1,i2,s,r,R12,R21,LR12,LR21,sz1,sz2,res;

debug:=false;
R12 := e1*R*e2;
R21 := e2*R*e1;

LR12:=AsList(R12);
LR21:=AsList(R21);
sz1:=Size(R12);
sz2:=Size(R21);
res:=false;

Print("\n");
for i1 in [1..sz1] do
	r:=LR12[i1];
	for i2 in [1..sz2] do
   	s:=LR21[i2];
   	if r*s=e1 and s*r=e2 then 
			res:=true; 	
			return res; ## !
			if debug then Print(" r=",r,",  s=",s,"\n"); fi;
		fi;
	od;
od;
if debug then Print("res=",res,"\n"); fi;
return res;
end;;

## SHOTER CODE FOR DISSER
IsIsomorphic := function ( R, e1, e2 )
local i1,i2,s,r,L12,L21;
L12:=AsList(e1*R*e2);	
L21:=AsList(e2*R*e1);
for i1 in [1..Size(L12)] do
	r:=L12[i1];
	for i2 in [1..Size(L21)] do
   	s:=L21[i2];
   	if r*s=e1 and s*r=e2 then 
			return true; 
		fi;
	od;
od;
return false;
end;;


## Checking whether two R-modules are isomorphic
## there exist s in ei*R*ek, t in ek*R*ei, that st=ei and ts=ek
IsomMax := function ( R, ei, ek, max )
local i,res1,res2,res,r1,r2,s,t;
res:=0;
Print("Start\n");
for i in [1..max] do
   res1:=0;
   res2:=0;
   r1:=rand_el(R);
   r2:=rand_el(R);       
   s:=ei*r1*ek;
   t:=ek*r1*ei;
   Print("i=",i);

   if s*t=ei then 
      Print("; s*t=ei ");
      res1:=1;      
   fi;

   if t*s=ek then 
      Print(" t*s=ek ");
      res2:=1;      
   fi;

   res:=res1*res2;

   if res=1 then
      Print("\n s="); StructR(s,R,1);
      Print("t="); StructR(t,R,1);   
      return res;
   fi;
   Print("\n");
od;

Print("res=",res,"\n");
return res;
end;;


######################
## Checking whether a ring is uniserial
UNS2 := function (R,i0,Le)
local eRe,ei,szL,i,j,k,ej,ek,ia,ib,ir,is,ir0,is0,s,r,a,b,eij,eik,Rij,Rik,Rjk,Rkj,LRij,LRik,LRjk,LRkj,szRij,szRik,szRkj,szRjk,res,res1,res2;

res:=1;  ## 1 - if ei*R is serial,  0 - if not.
szL := Size(Le);
Print("szL=",szL," \n");
ei:=Le[i0]; ## idemp of our module e_i0*R
 
eRe:=[];  # matrix szL*szL
for i in [1..szL] do
   eRe[i]:=[];
   for j in [1..szL] do   
      Print("start calc e",i,"*R*e",j," \n");
      ei:=Le[i];
      ej:=Le[j];
      eRe[i][j]:=ei*R*ej;           
      Print("Size of e",i,"*R*e",j," is ",Size(eRe[i][j])," \n");
   od;
od;   

i:=i0;
Print("i=",i,"\n");
for j in [1..szL] do
   ej:=Le[j];
   Rij := eRe[i][j];
   Print("Size of Rij is ",Size(Rij)," \n");

   for k in [1..szL] do      
      ek:=Le[k];
      Rik := eRe[i][k];
      Print("size of ei*R*ek is ", Size(Rik) ," \n");
      Print("j=",j,",k=",k,"\n");
      
      LRij := AsList(Rij);  szRij := Size(Rij);
      LRik := AsList(Rik);  szRik := Size(Rik);
      Rjk := eRe[j][k];
      Rkj := eRe[k][j];  
      LRjk := AsList(Rjk);  szRjk := Size(Rjk);
      LRkj := AsList(Rkj);  szRkj := Size(Rkj);
            
      Print("size of Rjk is ", szRjk ," \n");
      Print("size of Rkj is ", szRkj ," \n");

      for ia in [1..szRij] do
         a := LRij[ia]; 
         for ib in [1..szRik] do
            b := LRik[ib]; 
            res1 := 0;
            res2 := 0;

            ## check a*r = b 
            for ir in [1..szRjk] do
               r := LRjk[ir];
               if a*r=b then 
                  res1:=1;
                  ir0:=ir;
                  break;   
               fi;		
            od;

            ## check b*s=a 
            for is in [1..szRkj] do
               s := LRkj[is];
               if b*s=a then 
                  res2:=1;
                  is0:=is;
                  break;   
               fi;
            od;
  
            Print(" ia=",ia,", ib=",ib);
            if res1=1 then Print(", res1=",res1, ", ir=",ir0); fi;
            if res2=1 then Print(", res2=",res2, ", is=",is0); fi;
            if res1=0 and res2=0 then
               res:=0;  ## if non serial
               Print("Module is non serial");
               return 0;
            fi;
            Print("\n");
         od;  ## i4
      od;  ## i3
   od;  ## i2
od;  ## i1
Print("res=",res,"\n");
return res;
end;;


###########################################################
# Checking whether a ring R is serial

Serial2 := function (R,Le)
local eRe,ei,szL,i,j,k,ej,ek,ia,ib,ir,is,ir0,is0,s,r,a,b,eij,eik,Rij,Rik,Rjk,Rkj,LRij,LRik,LRjk,LRkj,szRij,szRik,szRkj,szRjk,res,res1,res2,res3,res4,debug;

debug:=false;
szL := Size(Le);
eRe:=[];  # matrix szL*szL

for i in [1..szL] do
   eRe[i]:=[];
   for j in [1..szL] do   
      Print("|e",i,"*R*e",j,"| = ");
      ei:=Le[i];
      ej:=Le[j];
      eRe[i][j]:=ei*R*ej;           
      Print(Size(eRe[i][j])," \n");
   od;
od;   

res4:=1;  ## 1 - if all ring is serial,  0 - if not.
for i in [1..szL] do	
   res3:=1;  ## 1 - if ei*R is uniseril,  0 - if not.
   for j in [1..szL] do		           
      for k in [1..szL] do         
            ei:=Le[i];  
            ej:=Le[j];
            ek:=Le[k];      
         Rij := eRe[i][j];
         Rik := eRe[i][k];
         Rjk := eRe[j][k];
         Rkj := eRe[k][j];           
         LRij := AsList(Rij);  szRij := Size(Rij);
         LRik := AsList(Rik);  szRik := Size(Rik);
         LRjk := AsList(Rjk);  szRjk := Size(Rjk);
         LRkj := AsList(Rkj);  szRkj := Size(Rkj);               
         Print("i=",i,", j=",j,", k=",k,"; "); ##
         Print("|Rjk|=", szRjk ,", |Rkj|=", szRkj ," \n");

         for ia in [1..szRij] do
            a := LRij[ia]; 
            for ib in [1..szRik] do
               b := LRik[ib]; 
               res1 := 0;
               res2 := 0;

               ## a*r = b ?
               for ir in [1..szRjk] do
                  r := LRjk[ir];
                  if a*r=b then 
                     res1:=1;
                     ir0:=ir;
                     break;   
                  fi;		
               od;

               ## b*s=a ?
               for is in [1..szRkj] do
                  s := LRkj[is];
                  if b*s=a then 
                     res2:=1;
                     is0:=is;
                     break;   
                  fi;		
               od;
					
					if debug then 
						Print(" ia=",ia,", ib=",ib,"  ");
						if res1=1 then Print(", res1=",res1, ", ir=",ir0," "); fi;
						if res2=1 then Print(", res2=",res2, ", is=",is0," "); fi;
						Print("\n");
					##else Print(".");
					fi;
					
               if res1=0 and res2=0 then    ## NON SERIAL
                  res3:=0;       
                  Print("for a_",ia," and b_",ib,", there aren't such r or s, result=",res3,"\n");
                  Print("The module e",i,"*R is NON UNISERIAL.\n");
                  return 0;
               fi;
               
            od;  ## i4
         od;  ## i3
      od;  ## i2
   od;  ## i1
   Print("res3=",res3,"\n");
   res4:=res4*res3;
od;  ## i0

Print("res4=",res4,"\n");
if res4=1 then Print("Ring is serial\n"); fi;
return res4;
end;;


#####################################
# Checking that a ring R is uniserial

IsUniserialRing := function ( R )
 local n,RL,a,b,r,s,i,j,k,res,res1;
 n:=Size(R);
 RL:=AsList(R);
 res:=1;
 for i in [1..n] do
   for j in [1..n] do
      a:=RL[i];   # forall a
      b:=RL[j];   # forall b
      Print("a=",i,", b=",j);

      res1:=0;
      # search of r
      for k in [1..n] do    
         r:=RL[k];
         if a*r=b then 
            Print(", r=",k); 
            res1:=1;              
         fi;
      od; 

      # search of s      
      for k in [1..n] do   
         s:=RL[k];
         if b*s=a then 
            Print(", s=",k);  
            res1:=1;             
         fi;
      od;       
      
      if res1=0 then 
         res:=0;
         Print(" No. Ring is not uniserial \n");
         return false;
      fi;
      
      Print("\n");
   od;  # j1
 od; # i1
 if res=1 then
   Print("Yes. Ring is uniserial. ");
   return true;
 fi;
 return res;
end;;



#######################
# Search of submodules in ei*R (if it is small)
FindSubmodule := function(ei,R)
   local szi,szk,szm,L,x,k,Rii;
   szi:=Size(ei*R); 
   L:=AsList(ei*R);
	Rii:=ei*R*ei;
   szm:=1; ## 
   for k in [1..szi] do
		x:=L[k];
      szk:=Size((ei*x)*R);  # the size of submodule rk*R
      Print("k=",k,", Size=",szk);
      if szk>1 and szk<szi then
         Print("- is submodule, x=");
         StructR(x,R,1);
         szm:=szk;
			if x in Rii and szk=81 then
				break;
			fi;
      else Print("\n");
      fi;
   od;  
   Print("Size of max submodule=",szm,"\n");
end;;


