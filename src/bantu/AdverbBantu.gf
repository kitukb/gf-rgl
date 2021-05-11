incomplete concrete AdverbBantu of Adverb = 
  CatBantu ** open CommonBantu, ResBantu, Prelude in {

  lin
  --PositAdvAdj a =  {s = table{Ag g n p => a.s! Advv }}; 
  ComparAdvAdj cadv a np = let agr = complAgr np.a
    in{
    s = table {AgP1  n =>  a.s !AAdj G1 n ++ cadv.s ++ np.s ! npNom;
               AgP2  n =>  a.s !AAdj G1 n ++ cadv.s ++ np.s ! npNom;
               AgP3 g n =>  a.s !AAdj g n ++ cadv.s ++ np.s ! npNom }
           } ;
  ComparAdvAdjS cadv a s = {
       s = table{AgP1 n  =>   a.s! AAdj G1 n  ++ cadv.s ++ s.s;
       AgP2 n =>   a.s! AAdj G1 n  ++ cadv.s ++ s.s;
       AgP3 g n  =>   a.s! AAdj g n  ++ cadv.s ++ s.s} 
      } ;
  PrepNP prep np = let agr = complAgr np.a
    in {s =table{
     AgP1  n =>case prep.isFused of { True =>(np.s ! Nom ++ cBind ++ prep.s1); False => prep.s!n!G1 ++ (np.s !  Nom) }; -- change NPoss to ni
     AgP2  n =>case prep.isFused of { True =>(np.s ! Nom ++ cBind ++prep.s1); False => prep.s!n!G1 ++ (np.s !  Nom) };
    AgP3 g n =>case prep.isFused of { True =>(np.s ! Nom++ cBind ++prep.s1); False => prep.s!n!g ++ (np.s !  Nom) }
    }} ; 
  AdAdv sub se =  { s=\\agr => se.s!agr ++ sub.s  } ;
  SubjS sub se =  { s=\\agr => sub.s ++ se.s} ;
  AdnCAdv cadv = {s = cadv.s ++ cadv.p} ;
  oper
  cBind : Str = Predef.BIND ;
}
