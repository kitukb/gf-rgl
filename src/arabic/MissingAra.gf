resource MissingAra = open GrammarAra, Prelude in {

-- temporary definitions to enable the compilation of RGL API
oper AdAdv : AdA -> Adv -> Adv = notYet "AdAdv" ;
oper AdVVP : AdV -> VP -> VP = notYet "AdVVP" ;
oper AdjOrd : Ord -> AP = notYet "AdjOrd" ;
oper AdnCAdv : CAdv -> AdN = notYet "AdnCAdv" ;
oper AdvCN : CN -> Adv -> CN = notYet "AdvCN" ;
oper AdvIAdv : IAdv -> Adv -> IAdv = notYet "AdvIAdv" ;
oper AdvS : Adv -> S -> S = notYet "AdvS" ;
oper BaseAP : AP -> AP -> ListAP = notYet "BaseAP" ;
oper BaseAdv : Adv -> Adv -> ListAdv = notYet "BaseAdv" ;
oper BaseNP : NP -> NP -> ListNP = notYet "BaseNP" ;
oper BaseRS : RS -> RS -> ListRS = notYet "BaseRS" ;
oper BaseS : S -> S -> ListS = notYet "BaseS" ;
oper CAdvAP : CAdv -> AP -> NP -> AP = notYet "CAdvAP" ;
oper CleftAdv : Adv -> S -> Cl = notYet "CleftAdv" ;
oper CleftNP : NP -> RS -> Cl = notYet "CleftNP" ;
oper ComparAdvAdj : CAdv -> A -> NP -> Adv = notYet "ComparAdvAdj" ;
oper ComparAdvAdjS : CAdv -> A -> S -> Adv = notYet "ComparAdvAdjS" ;
oper ComplVA : VA -> AP -> VP = notYet "ComplVA" ;
oper ComplVQ : VQ -> QS -> VP = notYet "ComplVQ" ;
oper ComplVS : VS -> S -> VP = notYet "ComplVS" ;
oper ConjAP : Conj -> ListAP -> AP = notYet "ConjAP" ;
oper ConjAdv : Conj -> ListAdv -> Adv = notYet "ConjAdv" ;
oper ConjNP : Conj -> ListNP -> NP = notYet "ConjNP" ;
oper ConjRS : Conj -> ListRS -> RS = notYet "ConjRS" ;
oper ConjS : Conj -> ListS -> S = notYet "ConjS" ;
oper ConsAP : AP -> ListAP -> ListAP = notYet "ConsAP" ;
oper ConsAdv : Adv -> ListAdv -> ListAdv = notYet "ConsAdv" ;
oper ConsNP : NP -> ListNP -> ListNP = notYet "ConsNP" ;
oper ConsRS : RS -> ListRS -> ListRS = notYet "ConsRS" ;
oper ConsS : S -> ListS -> ListS = notYet "ConsS" ;
oper DetNP : Det -> NP = notYet "DetNP" ;
oper EmbedQS : QS -> SC = notYet "EmbedQS" ;
oper EmbedS : S -> SC = notYet "EmbedS" ;
oper EmbedVP : VP -> SC = notYet "EmbedVP" ;
oper ExistIP : IP -> QCl = notYet "ExistIP" ;
oper ExistNP : NP -> Cl = notYet "ExistNP" ;
oper FunRP : Prep -> NP -> RP -> RP = notYet "FunRP" ;
oper GenericCl : VP -> Cl = notYet "GenericCl" ;
oper ImpPl1 : VP -> Utt = notYet "ImpPl1" ;
oper ImpersCl : VP -> Cl = notYet "ImpersCl" ;
oper PConjConj : Conj -> PConj = notYet "PConjConj" ;
oper PPartNP : NP -> V2 -> NP = notYet "PPartNP" ;
oper PredSCVP : SC -> VP -> Cl = notYet "PredSCVP" ;
oper ProgrVP : VP -> VP = notYet "ProgrVP" ;
oper ReflA2 : A2 -> AP = notYet "ReflA2" ;
oper ReflVP : VPSlash -> VP = notYet "ReflVP" ;
oper SentAP : AP -> SC -> AP = notYet "SentAP" ;
oper SentCN : CN -> SC -> CN = notYet "SentCN" ;
oper Slash2V3 : V3 -> NP -> VPSlash = notYet "Slash2V3" ;
oper SlashV2A : V2A -> AP -> VPSlash = notYet "SlashV2A" ;
oper SlashV2Q : V2Q -> QS -> VPSlash = notYet "SlashV2Q" ;
oper SlashV2S : V2S -> S -> VPSlash = notYet "SlashV2S" ;
oper SlashV2V : V2V -> VP -> VPSlash = notYet "SlashV2V" ;
oper SlashV2VNP : V2V -> NP -> VPSlash -> VPSlash = notYet "SlashV2VNP" ;
oper SlashVS : NP -> VS -> SSlash -> ClSlash = notYet "SlashVS" ;
oper SubjS : Subj -> S -> Adv = notYet "SubjS" ;
oper VocNP : NP -> Voc = notYet "VocNP" ;
oper pot3plus : Sub1000 -> Sub1000 -> Sub1000000 = notYet "pot3plus" ;

}
