--# -path=.:../../prelude

--Kikamba Resource Morphology
--
-- Benson Kituku 2017 -- 2018


interface MorphoBantu = DiffBantu ** open CommonBantu,ResBantu in {

flags 
  optimize=all ;
  coding=utf8 ;


 oper
 iregN :Str-> Str ->Cgender -> Noun= \man,men,g ->mkNoun man men g;
  mkNoun :Str-> Str ->Cgender -> Noun= \man,men,g ->  { -- for irregular noun
    s = table{Sg =>  man ; Pl => men } ;
    g =  g;
    } ; 
    regAAd : Str-> Str -> {s : AForm =>  Str} = \seo,seoo -> regAdj seo seoo;
 regA :Str->{s : AForm =>  Str}= \adj ->regAdj adj [];
 regVP run  = { 
      s =\\ ag,pol,tes,ant =>run.s1!pol!tes!ant!ag; 
      compl=\\_=> [];
      progV = run.progV;
      imp=\\po,imf => run.imp!po!imf; 
      inf= run.s!VInf };
  
}
