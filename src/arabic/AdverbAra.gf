concrete AdverbAra of Adverb = CatAra ** open ResAra, Prelude in {
  flags coding=utf8;

  lin
    PositAdvAdj a = {s = a.s ! APosit Masc Sg Indef Acc} ;
--    ComparAdvAdj cadv a np = {
--      s = cadv.s ++ a.s ! AAdv ++ "مِنْ" ++ np.s ! Gen
--      } ;
--    ComparAdvAdjS cadv a s = {
--      s = cadv.s ++ a.s ! AAdv ++ "تهَن" ++ s.s
--      } ;

    PrepNP prep np = {s = prep.s ++ np.s ! prep.c} ;

--    AdAdv = cc2 ;
--
   -- : Subj -> S -> Adv ;              -- when she sleeps
   SubjS subj s = {s = subj.s ++ s.s ! Subord} ;
--    AdvSC s = s ; --- this rule give stack overflow in ordinary parsing
--
--    AdnCAdv cadv = {s = cadv.s ++ "تهَن"} ;
--
}
