incomplete concrete AdjectiveBantu of Adjective = 
  CatBantu ** open CommonBantu, ResBantu, Prelude in {
  flags coding=utf8;
  lin

   PositA  a = {s = \\g,n => a.s !AAdj g n ;isPre = True } ;
   ComparA a np = {
      s = \\g,n => a.s  !AAdj g n  ++ conjThan  ++ np.s ! npNom ; 
      isPre = False} ;

    UseComparA a = {s = \\g,n=> a.s !AAdj g n;isPre = True};

     AdjOrd ord = { 
         s = \\g,n => ord.s ! g  ;
      isPre = True
      } ;

    CAdvAP ad ap np = {
      s = \\g,n => ad.s ++ ap.s !g! n ++ ad.p ++ np.s ! npNom ; 
      isPre = False
      } ;

    ComplA2 a np = {
      s = \\g,n => a.s  !AAdj g n  ++ a.c2 ++ np.s !  Nom; 
      isPre = False
      } ;
    ReflA2 a ={
     s = \\g,n => a.s !AAdj g n ++ a.c2  ++ reflPron ! AgP3 g n  ;
     } ;

    SentAP ap sc = {
      s = \\g,n => ap.s !g! n ++ sc.s ; 
      isPre = False
      } ;

    AdAP ada ap = {
      s = \\g,n =>   ap.s ! g ! n ++ ada.s;
      isPre = ap.isPre
      } ;

    UseA2 a2 = {s = \\g, n => a2.s !AAdj g n ;isPre = True } ;

   AdvAP ap adv = {s = \\g,n => ap.s ! g ! n ++ adv.s!AgP3 g n 
   ; isPre = False} ;


}