StructR := function(t,R,var)
   local sp,cf,n,i,k,kf,G,ListG,sF,sz;

   sF:=Size(UnderlyingField(R));
   G:=UnderlyingGroup(R);
   ListG:=AsList(G);
   sz:=Size(G); 

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



