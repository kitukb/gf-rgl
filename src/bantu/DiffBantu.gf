--1 Differences between Bantu languages

interface DiffBantu = open CommonBantu, Prelude in {
  flags coding=utf8 ;

param 
  Agr =   AgP1  Number | AgP2  Number  | AgP3  Cgender Number  ; 
  PronForm= Pers | Poss Number  Cgender;
  DetForm = Sub | Obj  Cgender ;
  IMPForm = Com | Pol ;
  VForm ;
  DForm ;
  AForm;
  VExte;


oper 
--fixclass: NPCase ->  Cgender ->  Cgender;

   Cgender : PType ;
  firstGender :  Cgender ; -- G1
  secondGender :  Cgender ; -- G2
  Noun : Type  = {s : Number  => Str ; g :  Cgender};
  CNoun : Type = {s,s2 : Number =>  Str ; g :  Cgender};
  AAgr : Type  = {g :  Cgender ; n : Number} ;
 

oper
   clitAgr : Agr -> {n : Number ; p : Person} = \a -> case a of {
    AgP3 _ n  => {n = n; p = P3} ;
     AgP2 n  => {n = n; p = P2} ;
    AgP1  n  => {n = n; p = P1} 
    } ;
  complAgr : Agr -> {g :  Cgender ; n : Number} = \a -> case a of {
    AgP3 g n  => {g = g; n = n} ;
     AgP2 n  => {g = G1; n = n} ;
    AgP1  n  => {g = G1; n = n} 
        } ;
  predetAgr : Agr -> {g :  Cgender} = \a -> case a of {
    AgP3 g  _ => {g = g} ;
     AgP2  _  => {g = G1} ;
    AgP1  _  => {g = G1} 
    } ;
  nounAgr : Agr -> {g :  Cgender ; n : Number ; p : Person} = \a -> case a of {
  AgP3 g n  => {g = g;n = n; p = P3} ;
  AgP2 n => {g = G1;n = n; p = P2} ;
  AgP1  n => {g = G1;n = n; p = P1} 
    
    } ; 

  detAgr : Agr -> {g :  Cgender ; p : Person} = \a -> case a of {
  AgP3 g _  => {g = g; p = P3} ;
  AgP2 _  => {g = G1; p = P2} ;
  AgP1  _  => {g = G1; p = P1} 
 
    } ;

toAgr : Cgender -> Number -> Person -> Agr = \g, n, p ->
    case p of {
      P1 => AgP1  n  ;
      P2 => AgP2  n  ;
      P3 => AgP3 g n 
    } ;

  agrG1 : Number -> Person -> Agr = \n,p -> 
    AgP1  n ;
  dapagr :  Cgender -> Person -> Agr = \g,p -> case p of {
    P3  =>  AgP3 g Sg ;
    P2  =>  AgP2 Sg ;
    P1  =>  AgP1 Sg } ;
  agrP3 :  Cgender -> Number -> Agr = \g,n ->
    AgP3 g n  ;

  aagr :  Cgender -> Number -> AAgr = \g,n ->
    {g = g ; n = n} ;

  ---- Conjunction Agreements----

  conjAgr : Number -> Agr -> Agr -> Agr = \n,xa,ya -> 
    let 
      x = nounAgr xa ; y = nounAgr ya
    in toAgr
     (conjGender x.g y.g) (conjNumber (conjNumber x.n y.n) n) 
       (conjPPerson x.p y.p) ;

      

  conjGender :  Cgender ->  Cgender ->  Cgender ;



oper
  conjThan  : Str ; --one of them in bantu
  conjThat  : Str ;
  superVery : Str ; -- one of bantu
  such : Str;
  reflPron : Agr => Str ; -- second of bantu.


oper
  ProunSgprefix :  Cgender -> Str ; 
  ProunPlprefix :  Cgender -> Str ; 
  Cardoneprefix  :  Cgender ->  Str;
 Cardtwoprefix  :  Cgender ->  Str;
 Allpredetprefix :  Cgender ->  Str;
 PrefixPlNom :  Cgender ->  Str;
 mkprefix,Ordprefix :  Cgender ->  Str;
 Cardprefix :  Cgender ->  Str ;
 --Mostpredetprefix :  Cgender ->  Str;
 --Adjpprefix :  Cgender -> Number ->   Str;
 --VowelAdjprefix:  Cgender -> Number ->   Str;
 -- ConsonantAdjprefix:  Cgender -> Number ->   Str;
}

