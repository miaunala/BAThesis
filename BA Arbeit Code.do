tab wave8

codebook wave8

keep if wave8==1

tab euRefVote, nolabel
recode euRefVote (2= .)(9999= .)(missing= .)
tab euRefVote

tab age
recode age (missing= .)

tab edlevel, nolabel
tab edlevel
recode edlevel (0=5)(1=4)(2=3)(3=2)(4=1)(5=0)(missing= .)
label variable edlevel "Highest achieved school-leaving qualification"
label define education_lev 0"Postgrad" 1"Undergraduate" 2"A-level" 3"GCSE A*-C" 4"GCSE D-G" 5"No qualifications"
label value edlevel education_lev
tab edlevel

tab gender, nolabel
recode gender (1=1)(2=0)(missing= .)
label define fe_ma 0"Female" 1"Male"
label value gender fe_ma
tab gender

tab hhincome
tab profile_gross_household, nolabel
recode profile_gross_household (17 9999= .)(15=1)(14=2)(13=3)(12=4)(11=5)(10=6)(9=7)(8=8)(7=9)(6=10)(5=11)(4=12)(3=13)(2=14)(1=15)(missing= .)
tab profile_gross_household
label define income_spec 1"£150,000 and over" 2"£100,000 to £149,999 per year" 3"£70,000 to £99,999 per year" 4"£60,000 to £69,999 per year" 5"£50,000 to £59,999 per year" 6"£45,000 to £49,999 per year" 7"£40,000 to £44,999 per year" 8"£35,000 to £39,999 per year"  9"£30,000 to £34,999 per year" 10"£25,000 to £29,999 per year" 11"£20,000 to £24,999 per year" 12"£15,000 to £19,999 per year" 13"£10,000 to £14,999 per year" 14"£5,000 to £9,999 per year" 15"under £5,000 per year"
label value profile_gross_household income_spec
tab profile_gross_household

tab econPersonalRetro, nolabel
recode econPersonalRetro (1=5)(2=4)(3=3)(4=2)(5=1)(9999= .)(missing= .)
label define econ_sit 1"Got a lot better" 2"Got a little better" 3"Stayed the same" 4"Got a little worse" 5"Got a lot worse"
label value econPersonalRetro econ_sit
tab econPersonalRetro

tab partyId, nolabel
recode partyId (3=0)(2=1)(1=2)(6=3)(9999 10 9 7 5 4= .)(missing= .)
tab partyId
label define parties 0"Liberal Democrat" 1"Labour" 2"Conservative" 3"UK Independence Party"
label value partyId parties
tab partyId

tab EUMIICategory, nolabel

clonevar econEUthrleg = EUMIICategory
tab econEUthrleg
recode econEUthrleg (8 9 10= 0)(4 6 15 30 32 39 56=1)(nonmissing= .)(missing= .), gen(reasEUrefEcTl) 
label define ecleg 0"Economy" 1"EU throughput legitimacy"
label value reasEUrefEcTl ecleg
tab reasEUrefEcTl

tab EUMIICategory
rename EUMIICategory econimmig
recode econimmig (8 9 10= 0)(29 40=1)(nonmissing= .)(missing= .), gen(reasEUrefEcIm)
label define ecIm 0"Economy" 1"Immigration"
label value reasEUrefEcIm ecIm
tab reasEUrefEcIm


//Logistic regression
logit euRefVote i.reasEUrefEcIm age i.gender edlevel profile_gross_household econPersonalRetro i.partyId [pweight=w8core]
logit euRefVote i.reasEUrefEcTl age i.gender edlevel profile_gross_household econPersonalRetro i.partyId [pweight=w8core]


//Diagnostic

//linear relationship between any continuous independent variables and the logit transformation of the dependent variable
//-> für jede ind var einzeln!

boxtid logistic euRefVote age profile_gross_household [pweight=w8core]

//=> drop age variable


//Outlier

//Econmy-Immigration
quietly logit euRefVote i.reasEUrefEcIm age i.gender edlevel profile_gross_household econPersonalRetro i.partyId
predict beta1, dbeta
scatter beta1 id, mlabel(id)
//=> same for with and without age
//Outlier: 1 -> id==42127
drop if id==42127

//Economy-throughput legitimacy
quietly logit  euRefVote i.reasEUrefEcTl age i.gender i.edlevel profile_gross_household econPersonalRetro i.partyId 
predict beta2, dbeta
scatter beta2 id, mlabel(id)



//Multicollinearity
//No correlation between independent variables >0,8
//EcIM
asdoc pwcorr reasEUrefEcIm age gender edlevel profile_gross_household econPersonalRetro partyId, star(all) replace nonum
//EcTl
asdoc pwcorr reasEUrefEcTl age gender edlevel profile_gross_household econPersonalRetro partyId, star(all) replace nonum

//VIF & condition index: VIF < 5; condition index < 30
//EcIM
collin reasEUrefEcIm age gender edlevel profile_gross_household econPersonalRetro partyId, corr  
//EcTl
collin reasEUrefEcTl age gender edlevel profile_gross_household econPersonalRetro partyId, corr 


// Coefficients
logit euRefVote i.reasEUrefEcIm age i.gender edlevel profile_gross_household econPersonalRetro i.partyId [pweight=w8core]
margins reasEUrefEcIm, atmeans
marginsplot

logit euRefVote i.reasEUrefEcTl age i.gender edlevel profile_gross_household econPersonalRetro i.partyId [pweight=w8core]
margins reasEUrefEcTl, atmeans
marginsplot

//Margins
logistic euRefVote i.reasEUrefEcIm age i.gender i.edlevel profile_gross_household econPersonalRetro i.partyId [pweight=w8core]
logistic euRefVote i.reasEUrefEcTl i.gender i.edlevel profile_gross_household econPersonalRetro i.partyId [pweight=w8core]



//APPENDIX

//End:
// Margins, falls besser zu interpretieren with testing for specification error

//1. Economy-Immigration
logistic euRefVote i.reasEUrefEcIm age i.gender i.edlevel profile_gross_household econPersonalRetro i.partyId [pweight=w8core]
//Hosmer-Lemeshow-test
logistic euRefVote i.reasEUrefEcIm i.gender i.edlevel profile_gross_household econPersonalRetro i.partyId
estat gof, group(10)

//2. Economy-EU throughput legitimacy
logistic euRefVote i.reasEUrefEcTl age i.gender i.edlevel profile_gross_household econPersonalRetro i.partyId [pweight=w8core]
//Hosmer-Lemeshow-test
logistic euRefVote i.reasEUrefEcTl age i.gender i.edlevel profile_gross_household econPersonalRetro i.partyId
estat gof, group(10)

//threshold for Hosmer Lemeshow test' Prob>chi2 > 0,05 -> more than 0.05 (see https://www.statisticshowto.com/hosmer-lemeshow-test/)