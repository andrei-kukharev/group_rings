// Checking the seriality of group rings.
// To run this code, use
// http://magma.maths.usyd.edu.au/calc/
// Проверяет групповое кольцо на полуцепность.
// Просмотриваются неразрешимые группы с циклич. силов. подгр.
// Диапазон значений порядка групп задается в конце файла.

// Если каждый проективный модуль в списке T является цепным, 
// то функция IsListOfUniserial возвращает true, иначе - false.
IsListOfUniserial := function(T)
  for  i  in  [1  ..  #T]  do
    P := T[i];
    S := P/JacobsonRadical(P); // вычисляем цоколь модуля P
    dim := Dimension(P);  // размерность модуля P
	 while (dim gt 0) do     
      J := JacobsonRadical(P);  // радикал Джекобсона
      dim := Dimension(J);   
      // Проверка разложимости фактор-модуля P/J в прямую сумму:  
      isdec := IsDecomposable(P/J); 
		// Если P/J разложим, то модуль T[i] не цепной:
      if isdec then 
        return false;
      end if;		
      P := J; // иначе переходим на следующую итерацию
    end while;	
  end for;
return true;
end function;

// Проверка полуцепности группового кольца FG, 
// где G - конечная группа, F - конечное поле.
IsSerialGroupRing := function(G, F)
  H := sub<G|>;	// тривиальная (единичная) подгруппа в G
  hom := CosetAction(G, H);  //действие G на смежных классах G по H
  G2 := hom(G);  // образ этого действия дает 
  // перестановочное представление исходной группы G
  M := PermutationModule(G2, F); // получаем перестановчный модуль
  T := IndecomposableSummands(M); // разложение модуля в прямую сумму // неразложимых модулей
  return IsListOfUniserial(T); 
end function;

//-------------------------

//---------------------

Test := function(n_from, n_to, k_from)

	for n in [n_from .. n_to] do
		FLn := Factorization(n);  // разложение на множители
		// сразу пропускаем вариант 2^m
		if (#FLn eq 1) and (FLn[1][1] eq 2) then continue; end if;
		max_k := NumberOfSmallGroups(n);
		
		for k in [k_from .. max_k] do
			G := SmallGroup(n,k);
			if IsAbelian(G) or not IsSimple(G) then	continue; end if; // абелевы гр. пропукаем

			//printf "\n[%3o, %3o] %12o is Simple and NonAbelian", n, k, GroupName(G);	
		
			for iFL in FLn do
				p := iFL[1];  // делитель
				if p eq 2 then continue; end if; // пропускаем p=2				
				P := SylowSubgroup(G,p);  // силовская p-подгр.				
				// выход если силовская не цикл. или  группа РАЗРЕШИМА
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


order_start := 60;  // с групп какого порядка начинать
order_end := 300;   // и до какого порядка
Test(order_start, order_end, 1);

