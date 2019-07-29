####
## выводит элемент t кольца R=KG
## в виде 1*g1+2*g2+...
## G - группа

# not depended on char K 
# var=1 - compact form
# var=2 - full form
# var=3 - for latex

StructR := function(t,R,var)
   local sp,cf,n,i,k,kf,G,ListG,sF,sz;

   sF:=Size(UnderlyingField(R)); ## = |F|
   G:=UnderlyingGroup(R);
   ListG:=AsList(G);
   sz:=Size(G);  ## = |G|   

   sp:=Support(t);
   n:=Size(sp);
   cf:=CoefficientsBySupport(t);
   for i in [1..n] do
      ##Print("i=",i,"\n");
      for k in [1..Size(G)] do
         if ListG[k]=sp[i] then
            if sF=2 then 
               if cf[i]=Z(2)^0 then kf:=1; fi;               
            fi;
				if sF=3 then 
               if cf[i]=Z(3)^0 then kf:=1; fi;
               if cf[i]=Z(3) then kf:=2; fi;
            fi;
            if sF=5 then 
            	if cf[i]=Z(5)^0 then kf:=1; fi;
            	if cf[i]=Z(5) then kf:=2; fi;
            	if cf[i]=Z(5)^3 then kf:=3; fi;
            	if cf[i]=Z(5)^2 then kf:=4; fi;
            fi;
				if sF=7 then 
            	if cf[i]=Z(7)^0 then kf:=1; fi;
            	if cf[i]=Z(7)^2 then kf:=2; fi;
            	if cf[i]=Z(7) then kf:=3; fi;
            	if cf[i]=Z(7)^4 then kf:=4; fi;
            	if cf[i]=Z(7)^5 then kf:=5; fi;
            	if cf[i]=Z(7)^3 then kf:=6; fi;
           	fi;  
				if sF=9 then
               if cf[i]=Z(3)^0 then kf:=1; fi;
               if cf[i]=Z(3) then kf:=2; fi;
               if cf[i]=Z(3^2) then kf:=3; fi;
               if cf[i]=Z(3^2)^2 then kf:=4; fi;
               if cf[i]=Z(3^2)^3 then kf:=5; fi;
               if cf[i]=Z(3^2)^5 then kf:=6; fi;
               if cf[i]=Z(3^2)^6 then kf:=7; fi;
               if cf[i]=Z(3^2)^7 then kf:=8; fi;
				fi;          
           	##Print("cf[i]=",cf[i],"\n");
            if var=1 then Print(kf,"*g",k," + "); fi;
            if var=2 then Print(kf,"*",ListG[k]," + "); fi;
            if var=3 then 
					if kf=1 then
						Print("g_{",k,"} + "); 
					else
						Print(kf," g_{",k,"} + "); 
					fi;
				fi;
            if var=4 then Print(kf,"*g[",k,"] + "); fi;
			fi;
      od;
   od;
   Print("\n");
end;;

# matrix on finite field
StructM22 := function(M,p,opt)
	local i,j,kf;
	for i in [1..2] do
		for j in [1..2] do
			if p=3 then 	
				if M[i][j]=0*Z(3) then kf:=0; fi;
				if M[i][j]=Z(3)^0 then kf:=1; fi;
				if M[i][j]=Z(3) then kf:=2; fi;
			fi;
			if opt=1 then Print(kf," "); fi;
			if opt=2 then Print(kf," "); fi;
		od;		
		if opt=1 then Print("\n"); fi;
		if opt=2 then Print("; "); fi;
	od;
	if opt=2 then Print("\n"); fi;
end;;

# for small Ring (R - whole ring, R1 - subring)
ListElementsRing := function(R,R1,var)
	local i,n,LR,max,x;
	max:=10000;
	n:=Size(R1);
	if n>max then Print("Very big ring\n"); return -1; fi;
	LR:=AsList(R1);
	for i in [1..n] do
		x:=LR[i];
		Print("x",i," = ");
		StructR(x,R,var);
	od;
end;;




####
## запись коэффициентов элемента в файл
## в виде 1 2 3 2 0 1 2
## with 0 for all char F;

CoefToFile := function(t,R,file)
   local sp,cf,m,i,k,kf,G,p,ListG,sz,res;

   p:=Size(UnderlyingField(R)); ## = char F
   G:=UnderlyingGroup(R);
   ListG:=AsList(G);
   sz:=Size(G);

   sp:=Support(t);
   m:=Size(sp);
   cf:=CoefficientsBySupport(t);   

   for i in [1..sz] do
		res:=0;
      for k in [1..m] do
         if sp[k]=ListG[i] then
            if p=3 then 
               if cf[k]=Z(3)^0 then kf:=1; fi;
               if cf[k]=Z(3) then kf:=2; fi;
            fi;
            if p=5 then 
            	if cf[k]=Z(5)^0 then kf:=1; fi;
            	if cf[k]=Z(5) then kf:=2; fi;
            	if cf[k]=Z(5)^3 then kf:=3; fi;
            	if cf[k]=Z(5)^2 then kf:=4; fi;
            fi;
            Print(kf,"*g",i," + ");
            AppendTo(file,kf," ");
            res:=1;
         fi;
      od;
      ## if not element in sp, then 0
		if res=0 then
      	Print("0*g",i," + ");
			AppendTo(file,0," ");
		fi;
   od;
   AppendTo(file,"\n\r");
   Print("\n");
end;;

ListElemFileAsCoef := function(ListEl,R,file)
	local n,i;
	n:=Size(ListEl);	
	PrintTo(file,n,"\n");
	for i in [1..n] do
		CoefToFile(ListEl[i],R,file);
	od;	
end;;



#################################################
#################################################

## СТАРЫЕ ВЕРСИИ 
### old versions:

## for K=GF(5), i.e. p = 5
Struct_p5 := function(t,G)
   local sp,cf,n,i,k,kf,ListG;
   sp:=Support(t);
   n:=Size(sp);
   cf:=CoefficientsBySupport(t);
   ListG:=AsList(G);
   for i in [1..n] do
      ##Print("i=",i,"\n");
      for k in [1..Size(G)] do
         if ListG[k]=sp[i] then
            if cf[i]=Z(5)^0 then kf:=1; fi;
            if cf[i]=Z(5) then kf:=2; fi;
            if cf[i]=Z(5)^3 then kf:=3; fi;
            if cf[i]=Z(5)^2 then kf:=4; fi;
            Print(kf,"*g",k," + ");
         fi;
      od;
   od;
   Print("\n");
end;;

# в виде перестановок
Struct2_p5 := function(t,G)
   local sp,cf,n,i,k,kf,ListG;
   sp:=Support(t);
   n:=Size(sp);
   cf:=CoefficientsBySupport(t);
   ListG:=AsList(G);
   for i in [1..n] do
      ##Print("i=",i,"\n");
      for k in [1..Size(G)] do
         if ListG[k]=sp[i] then
            ##Print("i=",i,"; cf=",cf[i],"\n");
            if cf[i]=Z(5)^0 then kf:=1; fi;
            if cf[i]=Z(5) then kf:=2; fi;
            if cf[i]=Z(5)^3 then kf:=3; fi;
            if cf[i]=Z(5)^2 then kf:=4; fi;
            Print(kf,"*",ListG[k]," + ");
         fi;
      od;
   od;
   Print("\n");
end;;

Struct2_p3 := function(t,G)
   local sp,cf,n,i,k,kf,ListG;
   sp:=Support(t);
   n:=Size(sp);
   cf:=CoefficientsBySupport(t);
   ListG:=AsList(G);
   for i in [1..n] do
      ##Print("i=",i,"\n");
      for k in [1..Size(G)] do
         if ListG[k]=sp[i] then
            ##Print("i=",i,"; cf=",cf[i],"\n");
            if cf[i]=Z(3)^0 then kf:=1; fi;
            if cf[i]=Z(3) then kf:=2; fi;
            Print(kf,"*",ListG[k]," + ");
         fi;
      od;
   od;
   Print("\n");
end;;

#######################

# char K = 3

Struct_p3 := function(t,G)
   local sp,cf,n,i,k,kf,ListG;
   sp:=Support(t);
   n:=Size(sp);
   cf:=CoefficientsBySupport(t);
   ListG:=AsList(G);
   for i in [1..n] do
      ##Print("i=",i,"\n");
      for k in [1..Size(G)] do
         if ListG[k]=sp[i] then
            if cf[i]=Z(3)^0 then kf:=1; fi;
            if cf[i]=Z(3) then kf:=2; fi;
            Print(kf,"*g",k," + ");
         fi;
      od;
   od;
   Print("\n");
end;;



#########################
## for Rationals;
Struct_R := function(t,G)
   local sp,cf,n,i,k,kf,ListG;
   sp:=Support(t);
   n:=Size(sp);
   cf:=CoefficientsBySupport(t);
   ListG:=AsList(G);
   for i in [1..n] do
      for k in [1..Size(G)] do
         if ListG[k]=sp[i] then
            Print(cf[i],"*g",k," + ");
         fi;
      od;
   od;
   Print("\n");
end;;

