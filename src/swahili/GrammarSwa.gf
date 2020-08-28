--# -path=.:../abstract:../common:prelude

concrete GrammarSwa of Grammar = 
  NounSwa, 
  VerbSwa, 
  AdjectiveSwa,
  AdverbSwa,
  NumeralSwa,
  SentenceSwa,
  QuestionSwa,
  RelativeSwa,
  ConjunctionSwa,
  PhraseSwa,
  TextX - [Adv],
  StructuralSwa,
  IdiomSwa,
  TenseX - [Adv]
  **  {

flags startcat = Phr ; unlexer = text ; lexer = text;

--lin
  --PPos = {s = [] ; p = CPos} ;
 -- PNeg = {s = [] ; p = CNeg True} ; -- contracted: don't
 --PPos  = {s = [] ; b = True} ;
 --  PNeg  = {s = [] ; b = False} ;
 --  TPres = {s = [] ; t = ResSwa.Pres} ;
 -- TPast = {s = [] ; t = ResSwa.Past };
 --  TFut = {s = [] ; t = ResSwa.Fut };
} ;
