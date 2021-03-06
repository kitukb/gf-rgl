incomplete concrete NounBantu of Noun = 
  
  CatBantu ** open Structural ,ResBantu, Prelude in {

flags optimize=all_subs ; coding = utf8 ;

lin
  -- Det -> CN -> NP
  -- e.g. 'the man'
  
  DetCN det cn = {s =\\c=> case det.isPre of {
     False =>  det.s!cn.g ++ cn.s ! det.n   ++ cn.s2!det.n;
     True => cn.s ! det.n  ++ det.s!cn.g ++ cn.s2!det.n};
    a =toAgr cn.g det.n P3 ; 
    isPron=False } ;

  --!(fixclass (npcase2case c) cn.g) 
  -- PN -> NP
  -- e.g. 'John'
  UsePN pn = {s =\\c => pn.s ; a = toAgr pn.g  Sg P3;isPron=False} ;
 
  -- Pron -> NP
  -- e.g. 'he'
  UsePron pron = let agr = nounAgr pron.a;
                     n=agr.n; g=agr.g 
    in {s =\\c => pron.s!Pers ;
             a = toAgr agr.g agr.n agr.p;
             isPron=True                             
    } ; 
  -- Predet -> NP -> NP
  -- e.g. 'only the man'
    
  PredetNP pred np = 
    let agr = nounAgr np.a  in {
      s = \\c =>   np.s !  Nom ++ pred.s ! agr.g   ;
      a =AgP3 agr.g agr.n  ;
      isPron=np.isPron
    } ; 
--PPartNP : NP -> V2  -> NP ;    -- the man seen
   PPartNP np v2 = {
      s = \\c =>  np.s ! c ++ subject np.a ++ bind ++ v2.s ! forms ;
      a = np.a;
      isPron=np.isPron
      } ;

 --  RelNP   : NP -> RS  -> NP ;    -- Paris, which is here  
  RelNP  np rs = {
    s = \\c => np.s !  Nom ++ frontComma ++ rs.s ! np.a ;--++ finalComma ;
    a = np.a;
    isPron=np.isPron
    } ;
  -- NP -> Adv -> NP
  -- e.g. 'Paris today'
 AdvNP np adv =  let  agr = nounAgr np.a 
                     in{
      s = \\c => np.s !  Nom ++ adv.s !AgP3 agr.g agr.n  ;
      a = np.a;
      isPron=np.isPron
      } ;
     --  ExtAdvNP: NP -> Adv -> NP ;    -- boys, such as ..
ExtAdvNP np adv = let  agr = nounAgr np.a 
                     in{
      --s = \\c => np.s !  Nom ++ adv.s ;
      s = \\c => np.s !  Nom ++ adv.s !AgP3  agr.g agr.n    ;
      a = np.a;
      isPron=np.isPron
      } ;

  DetQuant quant num = { s = \\g =>quant.s ! num.n! g   ++ num.s ! g; 
                            n  = num.n ; isPre =True} ;

  DetQuantOrd quant num ord ={ s = \\ g =>quant.s ! num.n! g  ++  num.s! g  ++ ord.s ! g; 
                          n  = num.n ; isPre =True } ;
  --DetNP   : Det -> NP 
 DetNP det = 
   { s = \\c => det.s!G1 ; 
       a = AgP3  G1 det.n ;
     isPron=False} ;  
  
  PossPron pron = { s = \\n,g => pron.s!Poss n g } ;
  
  NumSg = {s = \\_ => []; n = Sg };--; hasCard = False} ;
  NumPl = {s = \\_ => []; n = Pl };--; hasCard = False} ;
--b    NoOrd = {s = []} ;

  NumCard n = n  ;--** {hasCard = True} ;

  NumDigits n = {s = n.s ! NCard ; n = n.n} ;
  OrdDigits n = { s =  n.s ! NOrd} ;

  NumNumeral numeral = {s = numeral.s ! NCard; n = numeral.n} ;
  OrdNumeral numeral = {s = numeral.s ! NOrd} ;

  AdNum adn num = {s = \\g => adn.s ++ num.s!g ; n = num.n} ;

    OrdSuperl a ={s = \\g =>  a.s! AAdj g Sg ++ superVery} ;-- find how to include plular

 OrdNumeralSuperl n a = {s = \\g => n.s ! NOrd !g ++ a.s !AAdj g Sg } ;--what PL
   --  DefArt = {  s  = \\n,g => []} ; --what PL
  IndefArt, DefArt = {  s  = \\n,g =>[] } ;
     --IndefArt = {s = \\ n,g => artIndef } ;
    
  -- CN -> NP beer
  MassNP cn = let g = cn.g ; n = Sg | Pl in {
    s = \\c => cn.s ! n;
    a = AgP3 g n  ;
    isPron=False
    } ;
  UseN n = { s = n.s ; s2 = \\_ => [] ; g = n.g} ; --n
  UseN2 n = { s = n.s ; s2 = \\_ => [] ; g = n.g} ;--n ;
  UseN3 n = { s = n.s ; s2 = \\_ => [] ; g = n.g} ; --n ;

  Use2N3 f = {
    s = \\n => f.s ! n ;
    s2 = \\_ => [] ;
    g = f.g ;
    c2 = f.c2
    } ;

  Use3N3 f = {
    s = \\n => f.s ! n ;
    s2 = \\_ => [] ;
    g = f.g ;
    c2 = f.c3
    } ;

  ComplN2 n2 np = {s = \\n => n2.s ! n  ++ n2.c2.s!n!n2.g ++ np.s !  Nom ;
                   s2 = \\_ => [] ;
                   g = n2.g 
    }; 
  ComplN3 n3 np = {
    s = \\n => n3.s ! n  ++ n3.c2.s!n!n3.g ++ np.s !  Nom;
    g = n3.g ;
    c2 = n3.c3
    } ;

  AdjCN ap cn = {s = cn.s ; g = cn.g; s2 = \\n =>cn.s2! n ++ ap.s ! cn.g ! n} ;

 --RelCN   : CN -> RS  -> CN ;   -- house that John bought 
  RelCN cn rs = {
    s = \\n => cn.s ! n  ++ rs.s ! AgP3 cn.g n  ; s2 =\\n => []; --another persons
    g = cn.g
    } ; 
  -- AP -> CN -> CN
  -- e.g. 'big house'
  AdvCN cn ad = {s = \\n=> cn.s ! n ++ ad.s!AgP3 cn.g n  ;s2 =\\n => []; g = cn.g} ;

  SentCN cn sc = {s = \\n => cn.s ! n ++ sc.s ; s2 =\\n => []; g = cn.g} ;

  -- ApposCN cn np = {s = \\n,c => cn.s ! n ! Nom ++ np.s !  Nom ; s2 =\\n => []; g = cn.g} ;
 
  -- PossNP : CN -> NP -> CN
  -- e.g. 'house of Paris', 'house of mine'
  PossNP cn np =let agr = detAgr np.a in
    {s = \\n => cn.s ! n ++ possess_Prep.s! n!cn.g  ++ np.s ! NPoss; 
     s2 =\\n => []; g = cn.g} ;
  -- PartNP : CN -> NP -> CN
  -- e.g. 'glass of wine'
  PartNP cn np = {s = \\n => cn.s ! n  ++ part_Prep.s! n!cn.g  ++ np.s !  Nom ; s2 =\\n => []; g = cn.g} ;
    
  -- CountNP : Det -> NP -> NP
  -- e.g. 'three of them', 'some of the boys'
  CountNP det np = let  g = (predetAgr np.a).g 
    in {
      s = \\c => det.s!g ++ part_Prep.s!det.n!g ++ np.s!c ;--NPAcc was removed
      a = AgP3 g det.n ;
      isPron=np.isPron
    } ;

   --AdjDAP : DAP -> AP -> DAP
  AdjDAP det ap = { s = \\ Cgender =>det.s! Cgender ++ ap.s! Cgender !det.n; 
                    n = det.n; isPre=det.isPre  };
      
  DetDAP d = { s=d.s; n=d.n; isPre=d.isPre};
    
 ApposCN cn np = let agr = complAgr np.a in  {s = \\n => np.s !  Nom    ++ cn.s !n    ; s2 =\\n => ""; g = cn.g} ;
 --++ possess_Prep.s!n!agr.g  ; 
  oper
bind : Str = Predef.BIND ;
}
