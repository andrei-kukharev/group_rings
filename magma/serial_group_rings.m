// Checking the seriality of group rings.
// To run this code: http://magma.maths.usyd.edu.au/calc/

// If each projective module in the list T is uniserial,
// then the function IsListOfUniserial() returns True, otherwise False.
IsListOfUniserial := function(T)
  for  i  in  [1  ..  #T]  do
    P := T[i];
    S := P/JacobsonRadical(P); // socle of R
    dim := Dimension(P);  // dim of P
	 while (dim gt 0) do     
      J := JacobsonRadical(P);
      dim := Dimension(J);   
      // Checking the decomposability of  P/J f into a direct sum
      isdec := IsDecomposable(P/J); 
      if isdec then 
        return false;
      end if;		
      P := J; 
    end while;	
  end for;
return true;
end function;


// Checking the seriality of the group ring FG
// where G is a finite group, and F is a finite field.
IsSerialGroupRing := function(G, F)
  H := sub<G|>;	
  hom := CosetAction(G, H);  
  G2 := hom(G);  
  M := PermutationModule(G2, F); 
  T := IndecomposableSummands(M);
  return IsListOfUniserial(T); 
end function;


// Searching serial group rings
Test := function(n_from, n_to, k_from)

	for n in [n_from .. n_to] do
		FLn := Factorization(n);  
		if (#FLn eq 1) and (FLn[1][1] eq 2) then continue; end if;
		max_k := NumberOfSmallGroups(n);
		
		for k in [k_from .. max_k] do
			G := SmallGroup(n,k);
			if IsAbelian(G) or not IsSimple(G) then	continue; end if; 
			for iFL in FLn do
				p := iFL[1];  
				if p eq 2 then continue; end if; // pass if p=2
				P := SylowSubgroup(G,p);  
				if not IsCyclic(P) or IsSolvable(G) then continue; end if; 
				printf "\n[%3o, %3o] %12o, p=%2o: ", n, k, GroupName(G), p;	
				F:=GF(p);
				res := IsSerialGroupRing(G,F);
				if res then printf "yes"; else printf "-"; end if;			
				end for;
		end for;
	end for;
	printf "\n";
	return 1;
end function;

order_start := 60; 
order_end := 300;  
Test(order_start, order_end, 1);

