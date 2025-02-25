cd "C:\Users\Radhika\Desktop\Radhika Project"

use "Project Data"

tsset tid, quarterly

log using "out1.smcl", replace

// DENMARK 
// DF unit root tests: may use different lags
dfuller denr, trend regress lags(2)
*The p-value for the test is greater than the significance level of 0.05, so we cannot reject the null hypothesis. 

dfuller denw, trend regress lags(2)
*the test statistic is -2.052, which is less than the critical values at all levels of significance. Therefore, we fail to reject the null hypothesis

// FRANCE 
// DF unit root tests: may use different lags
dfuller frar, trend regress lags(2)
*Since the ADF test statistic is higher than the critical values at all these significance levels, we cannot reject the null hypothesis.f
dfuller fraw, trend regress lags(2)
*Since the p-value is greater than 0.05, we fail to reject the null hypothesis 

// NETHERLANDS  
// DF unit root tests: may use different lags
dfuller netr, trend regress lags(2)
*Not Stationary, we fail we reject the null.
dfuller netw, trend regress lags(2)
*Not Stationary, we fail we reject the null.

// Switzerland 
// DF unit root tests: may use different lags
dfuller swir, trend regress lags(2)
*Not Stationary, we fail we reject the null.
dfuller swiw, trend regress lags(2)
*Not Stationary, we fail we reject the null.

// United Kingdom 
// DF unit root tests: may use different lags
dfuller ukr, trend regress lags(2)
*Not Stationary, we fail we reject the null.
dfuller ukw, trend regress lags(2)
*Not Stationary, we fail we reject the null.
 
Denmark

reg denr denw
*This is a linear regression analysis with denw as the independent variable and denr as the dependent variable. 

estat hettest, iid 
*H0 is rejected ==> heteroskedasticity 

estat bgodfrey, lags(4)
*H0 is rejected ==> serial correlation exists :( (but we are anyway testing for lags?)

estat ovtest
*H0 is not rejected at 5% significance ==> no omitted variable :)

estat archlm, lags(4)
*H0 is not rejected ==> no ARCH effects exist ,ARCH(p) disturbance ==> :( 

predict bdehat, resid
dfuller bdehat, noconstant regress lags(2) 
*H0 is not rejected ==> non-stationary

FRANCE
reg frar fraw

estat hettest, iid
*H0 is not rejected ==> no heteroskedasticity 

estat bgodfrey, lags(4)
*H0 is rejected ==> serial correlation exists 

estat ovtest
*H0 is not rejected ==> no omitted variable 

estat archlm, lags(4)
*H0 is rejected ==> ARCH effects exist ,ARCH(p) disturbance 

predict bfrhat, resid
dfuller bfrhat, noconstant regress lags(2) 
*H0 is not rejected ==> non-stationary

NETHERLANDS
reg netr netw

estat hettest, iid
*H0 is not rejected ==> no heteroskedasticity 

estat bgodfrey, lags(4)
*H0 is rejected ==> serial correlation exists 

estat ovtest
*H0 is not rejected at significance ==> no omitted variable 

estat archlm, lags(4)
*H0 is rejected ==> ARCH effects exist ,ARCH(p) disturbance

predict bnehat, resid
dfuller bnehat, noconstant regress lags(2) 
*H0 is not rejected ==> non-stationary

SWITZERLAND
reg swir swiw

estat hettest, iid
*H0 is not rejected ==> heteroskedasticity 

estat bgodfrey, lags(4)
*H0 is rejected ==> serial correlation exists 

estat ovtest
*H0 is not rejected at 1% significance ==> no omitted variable 

estat archlm, lags(4)
*H0 is not rejected ==> no ARCH effects exist ,ARCH(p) disturbance 

predict bswhat, resid
dfuller bswhat, noconstant regress lags(2) 
*H0 is not rejected ==> non-stationary

UNITED KINGDOM
reg ukr ukw

estat hettest, iid
*H0 is not rejected ==> no heteroskedasticity 

estat bgodfrey, lags(4)
*H0 is rejected ==> serial correlation exists 

estat ovtest
*H0 is not rejected at 1% significance ==> no omitted variable 

estat archlm, lags(4)
*H0 is rejected ==> no ARCH(p) disturbance ==> :

predict bukhat, resid
dfuller bukhat, noconstant regress lags(2) 
*H0 is not rejected ==> non-stationary

// robust OLS estimation
Denmark
reg denr denw, robust
// static regression with dummy and interaction variable
// reg retail wholesale dummy1 dummy2
*dummies - // generation of the dummy variable - SOME IMPORTANT CHANGES, EXPRESSED IN SAME CURRENCY. 
*eudum =1 IF POSITIVE CHANGE IN SPOT PRICE IN EURO  (EUDUM = EUro + DUMmy)
*ge eudum = 0
*ge daraeu = D.araeu
*replace eudum = 1 if daraeu > 0
*ge araeudum = araeu*eudum  THIS IS AN INTERACTION VARIABLE, WHICH SHOWS INTERACTION OF SPOTPRICE AND ITS DUMMY

ge denwdum = 0
ge ddenw = D.denw
replace denwdum = 1 if ddenw > 0
ge indenw = denw*denwdum
interaction 

reg denr denw denwdum indenw 

estat hettest, iid
*H0 is rejected ==> no heteroskedasticity 
estat bgodfrey, lags(4)
*H0 is not rejected ==> serial correlation exists 
estat ovtest
*H0 is not rejected  ==> omitted variable 
estat archlm, lags(4)
*H0 is rejected ==>  ARCH effects exist ,ARCH(p) disturbance 
predict bhat1, resid
dfuller bhat1, noconstant regress lags(2)
reg denr denw denwdum indenw , robust
*Not Significant 

France
reg frar fraw, robust
ge frawdum = 0
ge dfraw = D.fraw
replace frawdum = 1 if dfraw > 0
ge infraw = fraw*frawdum
reg frar fraw frawdum infraw 
estat hettest, iid
*H0 is not rejected ==> heteroskedasticity 
estat bgodfrey, lags(4)
*H0 is rejected ==> serial correlation exists 
estat ovtest
*H0 is not rejected  ==> omitted variable 
estat archlm, lags(4)
*H0 is rejected ==>  no ARCH effects exist ,ARCH(p) disturbance 
predict bfrhat1, resid
dfuller bfrhat1, noconstant regress lags(2)
reg frar fraw frawdum infraw, robust
*H0 is not rejected ==> non-stationary

Netherlands
reg netr netw, robust
ge netwdum = 0
ge dnetw = D.netw
replace netwdum = 1 if dnetw > 0
ge innetw = netw*netwdum
reg netr netw netwdum innetw 
estat hettest, iid
*H0 is rejected ==> heteroskedasticity :(
estat bgodfrey, lags(4)
*H0 is rejected ==> serial correlation exists 
estat ovtest
*H0 is not rejected ==> no omitted variable 
estat archlm, lags(4)
*H0 is rejected ==>  ARCH effects exist ,ARCH(p) disturbance ==> :)
predict bnehat1, resid
dfuller bnehat1, noconstant regress lags(2)
reg netr netw netwdum innetw, robust
*H0 is not rejected ==> non-stationary

Switzerland
reg swir swiw, robust
ge swiwdum = 0
ge dswiw = D.swiw
replace swiwdum = 1 if dswiw > 0
ge inswiw = swiw*swiwdum
reg swir swiw swiwdum inswiw 
estat hettest, iid
*H0 is not rejected ==> heteroskedasticity 
estat bgodfrey, lags(4)
*H0 is rejected ==> serial correlation exists 
estat ovtest
*H0 is not rejected at ==> omitted variable 
estat archlm, lags(4)
*H0 is not rejected ==>  ARCH effects exist ,ARCH(p) disturbance 
predict bswhat1, resid
dfuller bswhat1, noconstant regress lags(2)
reg swir swiw swiwdum inswiw, robust
*H0 is not rejected ==> non-stationary

United Kingdom
reg ukr ukw, robust
ge ukwdum = 0
ge dukw = D.ukw
replace ukwdum = 1 if dukw > 0
ge inukw = ukw*ukwdum
reg ukr ukw ukwdum inukw 
estat hettest, iid
*H0 is rejected ==> heteroskedasticity :(
estat bgodfrey, lags(4)
*H0 is rejected ==> serial correlation exists 
estat ovtest
*H0 is not rejected ==> omitted variable 
estat archlm, lags(4)
*H0 is rejected ==> no ARCH effects exist ,ARCH(p) disturbance 
predict bukhat1, resid
dfuller bukhat1, noconstant regress lags(2)
reg ukr ukw ukwdum inukw, robust
*H0 is not rejected ==> non-stationary

// dynamic regression: you may add more lags if necessary
*reg dbel L.BELGAS L.araeu L(1/1).dbel L(0/1).daraeu 
*REG DIFFERENCE_RETAIL L.RETAIL L.WHOLESALE L(1/1).DIFFERENCE_RETAIL L(0/1).DIFFERENCE_WHOLESALE
*estat hettest, iid
*H0 - homoskedasticity - rejected. 
*estat bgodfrey, lags(4)
*estat ovtest
*estat archlm, lags(4)
// PSS F test for cointegration
*test (L.BELGAS=0)(L.araeu=0) 
*reg dbel L.BELGAS L.araeu L(1/1).dbel L(0/1).daraeu, robust

*GENERATING RETAIL DIFFERENCE VARIABLES
ge ddenr = D.denr
ge dfrar = D.frar
ge dnetr = D.netr
ge dswir = D.swir
ge dukr = D.ukr

*GENERATING WHOLESALE DIFFERENCE VARIABLES
ge ddenw = D.denw
ge dfraw = D.fraw
ge dnetw = D.netw
ge dswiw = D.swiw
ge dukw = D.ukw

*Denmark
reg ddenr L.denr L.denw L(1/1).ddenr L(0/1).ddenw

estat hettest, iid
*H0 is not rejected  - no heteroskedasticity :) 
estat bgodfrey, lags(4)
*H0 is not rejected  - no serial correlation :) 
estat ovtest
*H0 is not rejected - omitted variable
estat archlm, lags(4)
*H0 is not rejected - no ARCH effects :) 
// PSS F test for cointegration
test (L.denr=0)(L.denw=0)
*H0 is not rejected, no cointegration
reg ddenr L.denr L.denw L(1/1).ddenr L(0/1).ddenw, robust
*not significant

*France
reg dfrar L.frar L.fraw L(1/1).dfrar L(0/1).dfraw

estat hettest, iid
*H0 is not rejected - no heteroskedasticity 
estat bgodfrey, lags(4)
*H0 is rejected - no serial correlation 
estat ovtest
*H0 is not rejected - omitted variable 
estat archlm, lags(4)
*H0 is not rejected - no ARCH effects 
// PSS F test for cointegration
test (L.frar=0)(L.fraw=0)
*H0 is not rejected, no cointegration
reg dfrar L.frar L.fraw L(1/1).dfrar L(0/1).dfraw, robust


*Netherlands
reg dnetr L.netr L.netw L(1/1).dnetr L(0/1).dnetw

estat hettest, iid
*H0 is rejected - no heteroskedasticity :(
estat bgodfrey, lags(4)
*H0 is not rejected at  - no serial correlation :) 
estat ovtest
*H0 is rejected - omitted variable
estat archlm, lags(4)
*H0 is not rejected - no ARCH effects :) 
// PSS F test for cointegration
test (L.netr=0)(L.netw=0)
*H0 is not rejected , no cointegration
reg dnetr L.netr L.netw L(1/1).dnetr L(0/1).dnetw, robust


*Switzerland
reg dswir L.swir L.swiw L(1/1).dswir L(0/1).dswiw

estat hettest, iid
*H0 is rejected at 1% - no heteroskedasticity :( 
estat bgodfrey, lags(4)
*H0 is not rejected at 1% - no serial correlation :) 
estat ovtest
*H0 is not rejected - no omitted variable
estat archlm, lags(4)
*H0 is not rejected - no ARCH effects :) 
// PSS F test for cointegration
test (L.swir=0)(L.swiw=0)
*H0 is rejected, cointegration 
reg dswir L.swir L.swiw L(1/1).dswir L(0/1).dswiw, robust


*United Kingdom
reg dukr L.ukr L.ukw L(1/1).dukr L(0/1).dukw

estat hettest, iid
*H0 is rejected at 1% - no heteroskedasticity :( 
estat bgodfrey, lags(4)
*H0 is not rejected at 1% - no serial correlation :) 
estat ovtest
*H0 is not rejected - no omitted variable 
estat archlm, lags(4)
*H0 is not rejected - no ARCH effects :) 
// PSS F test for cointegration
test (L.ukr=0)(L.ukw=0)
*H0 is rejected, cointegration
reg dnetr L.ukr L.ukw L(1/1).dukr L(0/1).dukw, robust


// dynamic regression with dummy and interaction variable - THIS IS GOOD (OBO DIAGNOSTICS)
*l. MEANS 1ST LAG. L(0/1) MEANS 0TH AND 1ST LAG AND SO ON.
*reg dbel L.BELGAS L.araeu L(1/1).dbel L(0/1).daraeu L.BELGASdum L.araeudum L(1/1).dbeldum L(0/1).daraeudum
*REG DIFFERENCE_RETAIL L.RETAIL L.WHOLESALE L(1/1).DIFFERENCE_RETAIL L(0/1).DIFFERENCE_WHOLESALE L.RETAILDUMMY L.WHOLESALE_DUMMY L(1/1).DIFFERENCE_RETAIL_DUMMY L(0/1).DIFFERENCE_WHOLESALE_DUMMY
*estat hettest, iid
*estat bgodfrey, lags(4)
*estat ovtest
*estat archlm, lags(4)
*reg dbel L.BELGAS L.araeu L(1/1).dbel L(0/1).daraeu L.BELGASdum L.araeudum L(1/1).dbeldum L(0/1).daraeudum, robust
*ge BELGASdum = BELGAS*eudum
*ge RETAILDUMMY = RETAIL*WHOLESALE_DUMMY (BECAUSE SIGN OF CHANGE OF WHOLEALE MATTERS - ROCKET VSFEATHER )
*ge dbeld777um = dbel*eudum
*ge DIFFERENCE_RETAIL_DUMMY = DIFFERENCE_RETAIL*WHOLESALE_DUMMY  (SIMILAR TO ABOVE)
*ge DIFFERENCE_WHOLESALE_DUMMY = DIFFERENCE_WHOLESALE*WHOLESALE_DUMMY

Generating dummy and interaction variable
ge eudum = 0

ge denrdum = denr*eudum
ge frardum = frar*eudum
ge netrdum = netr*eudum
ge swirdum = swir*eudum
ge ukrdum = ukr*eudum
ge denwdum = denw*eudum
ge frawdum = fraw*eudum
ge netwdum = netw*eudum
ge swiwdum = swiw*eudum
ge ukwdum = ukw*eudum


ge ddenrdum = ddenr*eudum
ge dfrardum = dfrar*eudum
ge dnetrdum = dnetr*eudum
ge dswirdum = dswir*eudum
ge dukrdum = dukr*eudum
ge ddenwdum = ddenw*eudum
ge dfrawdum = dfraw*eudum
ge dnetwdum = dnetw*eudum
ge dswiwdum = dswiw*eudum
ge dukwdum = dukw*eudum


Denmark 
// dynamic regression with d
*l. MEANS 1ST LAG. L(0/1) MEANS 0TH AND 1ST LAG AND SO ON.
reg ddenr L.denr L.denw L(1/1).ddenrdum L(0/1).ddenw L.denrdum L.denwdum L(1/1).ddenrdum L(0/1).ddenwdum
estat hettest, iid
**H0 is not rejected - no heteroskedasticity. :)
estat bgodfrey, lags(4)
*H0 is not rejected - no serial correlation :)
estat ovtest
*H0 is rejected - no omitted variable. :(
estat archlm, lags(4)
*H0 is not rejected - no ARCH effects :)
reg ddenr L.denr L.denw L(1/1).ddenr L(0/1).ddenw L.denrdum L.denwdum L(1/1).ddenrdum L(0/1).ddenwdum, robust
 
FRANCE 
reg dfrar L.frar L.fraw L(1/1).dfrar L(0/1).dfraw L.frardum L.denwdum L(1,1).dfrardum L(0/1).dfrawdum
estat hettest, iid
*H0 is not rejected - no heteroskedasticity. :)
estat bgodfrey, lags(4)
*H0 is rejected -  serial correlation :)
estat ovtest
*H0 is not rejected - no omitted variable. :)
estat archlm, lags(4)
*H0 is not rejected - no ARCH effects :) 
reg dfrar L.frar L.fraw L(1/1).dfrar L(0/1).dfraw L.frardum L.denwdum L(1,1).dfrardum L(0/1).dfrawdum, robust
*R^2 is good. L1dfrret is stat sig at 5%. dfrwho is stat sig at 1%. L1dfrwho is stat sig at 1%. dwfrdum is stat sig at 5%.L1dwfrdum is stat sig at 5%. nothing else is stat sig.

NETHERLANDS 
reg dnetr L.netr L.netw L(1/1).dnetr L(0/1).dnetw L.netrdum L.netwdum L(1,1).dnetrdum L(0/1).dnetwdum
estat hettest, iid
*H0 is rejected - no heteroskedasticity. :(
estat bgodfrey, lags(4)
*H0 is not rejected - no serial correlation :)
estat ovtest
*H0 is not rejected - no omitted variable. :)
estat archlm, lags(4)
*H0 is not rejected - no ARCH effects :) 
reg dnetr L.netr L.netw L(1/1).dnetr L(0/1).dnetw L.netrdum L.netwdum L(1,1).dnetrdum L(0/1).dnetwdum, robust
*R^2 is good. L1dfrret is stat sig at 5%. dfrwho is stat sig at 1%. L1dfrwho is stat sig at 1%. dwfrdum is stat sig at 5%.L1dwfrdum is stat sig at 5%. nothing else is stat sig.

SWITZERLAND
reg dswir L.swir L.swiw L(1/1).dswir L(0/1).dswiw L.swirdum L.swiwdum L(1,1).dswirdum L(0/1).dswiwdum
estat hettest, iid
*H0 is rejected - no heteroskedasticity. :(
estat bgodfrey, lags(4)
*H0 is not rejected - no serial correlation :)
estat ovtest
*H0 is not rejected - omitted variable. 
estat archlm, lags(4)
*H0 is not rejected - no ARCH effects :) 
reg dswir L.swir L.swiw L(1/1).dswir L(0/1).dswiw L.swirdum L.swiwdum L(1,1).dswirdum L(0/1).dswiwdum, robust

UNITED KINGDOM 
reg dukr L.ukr L.ukw L(1/1).dukr L(0/1).dukw L.ukrdum L.ukwdum L(1,1).dukrdum L(0/1).dukwdum
estat hettest, iid
*H0 is not rejected - heteroskedasticity. 
estat bgodfrey, lags(4)
*H0 is not rejected - serial correlation 
estat ovtest
*H0 is not rejected - no omitted variable. :)
estat archlm, lags(4)
*H0 is not rejected - no ARCH effects :) 
reg dukr L.ukr L.ukw L(1/1).dukr L(0/1).dukw L.ukrdum L.ukwdum L(1,1).dukrdum L(0/1).dukwdum, robust

Denmark
*ARAEU - SPOT PRICE PLUS EURO_TO_USD RATE, THEN CHANGE IN THAT. (WE ADD BECAUSE BOTH ARE LOG VALUES)
ge pddenw = max(ddenw,0) 
*Positive wholesale
ge nddenw = min(ddenw,0)
ge pdenw = sum(pddenw)
ge ndenw = sum(nddenw)

ge pddenr = max(ddenr,0)
ge nddenr = min(ddenr,0)
ge pdenr = sum(pddenr)
ge ndenr = sum(nddenr)

// asymmetric static regression
reg denw pdenr ndenr
estat hettest, iid
*H0 is rejected - heteroskedasticity. 
estat bgodfrey, lags(4)
*H0 is rejected - serial correlation 
estat ovtest
*H0 is rejected - omitted variable. 
estat archlm, lags(4)
*H0 is not rejected - no ARCH effects
predict adehat, resid
dfuller adehat, noconstant regress lags(2) 
*not rejected

France
*ARAEU - SPOT PRICE PLUS EURO_TO_USD RATE, THEN CHANGE IN THAT. (WE ADD BECAUSE BOTH ARE LOG VALUES)

ge pdfraw = max(dfraw,0)
ge ndfraw = min(dfraw,0)
ge pfraw = sum(pdfraw)
ge nfraw = sum(ndfraw)
ge pdfrar = max(dfrar,0)
ge ndfrar = min(dfrar,0)
ge pfrar = sum(pdfrar)
ge nfrar = sum(ndfrar)

// asymmetric static regression
reg fraw pfrar nfrar
estat hettest, iid
*H0 is not rejected - no heteroskedasticity. 
estat bgodfrey, lags(4)
*H0 is not rejected - no serial correlation 
estat ovtest
*H0 is not rejected -no omitted variable
estat archlm, lags(4)
*H0 is not rejected -no ARCH effects
predict afrhat, resid
dfuller afrhat, noconstant regress lags(2) 
* Stationary

Netherlands
*ARAEU - SPOT PRICE PLUS EURO_TO_USD RATE, THEN CHANGE IN THAT. (WE ADD BECAUSE BOTH ARE LOG VALUES)

ge pdnetw = max(dnetw,0)
ge ndnetw = min(dnetw,0)
ge pnetw = sum(pdnetw)
ge nnetw = sum(ndnetw)

ge pdnetr = max(dnetr,0)
ge ndnetr = min(dnetr,0)
ge pnetr = sum(pdnetr)
ge nnetr = sum(ndnetr)

// asymmetric static regression
reg netw pnetr nnetr
estat hettest, iid
*H0 is not rejected - no heteroskedasticity. 
estat bgodfrey, lags(4)
*H0 is rejected - serial correlation 
estat ovtest
*H0 is not rejected -no omitted variable
estat archlm, lags(4)
*H0 is rejected - no ARCH effects
predict anehat, resid
dfuller anehat, noconstant regress lags(2) 
*not rejected

Switzerland
*ARAEU - SPOT PRICE PLUS EURO_TO_USD RATE, THEN CHANGE IN THAT. (WE ADD BECAUSE BOTH ARE LOG VALUES)

ge pdswiw = max(dswiw,0)
ge ndswiw = min(dswiw,0)
ge pswiw = sum(pdswiw)
ge nswiw = sum(ndswiw)

ge pdswir = max(dswir,0)
ge ndswir = min(dswir,0)
ge pswir = sum(pdswir)
ge nswir = sum(ndswir)

// asymmetric static regression
reg swiw pswir nswir
estat hettest, iid
*H0 is rejected - heteroskedasticity. 
estat bgodfrey, lags(4)
*H0 is not rejected - no serial correlation 
estat ovtest
*H0 is rejected - omitted variable. 
estat archlm, lags(4)
*H0 is not rejected - no ARCH effects
predict aswhat, resid
dfuller aswhat, noconstant regress lags(2) 
*rejected

United Kingdom
*ARAEU - SPOT PRICE PLUS EURO_TO_USD RATE, THEN CHANGE IN THAT. (WE ADD BECAUSE BOTH ARE LOG VALUES)

ge pdukw = max(dukw,0)
ge ndukw = min(dukw,0)
ge pukw = sum(pdukw)
ge nukw = sum(ndukw)

ge pdukr = max(dukr,0)
ge ndukr = min(dukr,0)
ge pukr = sum(pdukr)
ge nukr = sum(ndukr)

// asymmetric static regression
reg ukw pukr nukr
estat hettest, iid
*H0 is rejected - heteroskedasticity. 
estat bgodfrey, lags(4)
*H0 is rejected - serial correlation :(
estat ovtest
*H0 is not rejected ,no omitted variable
estat archlm, lags(4)
*H0 is not rejected - no ARCH effects
predict aukhat, resid
dfuller aukhat, noconstant regress lags(2) 
*not rejected


Denmark
// asymmetric dynamic regression: you may add more lags if necessary
reg ddenr L.denr L.pdenw L.ndenw L(1/1).ddenr L(0/1).pddenw L(0/1).nddenw 
//PSS F TEST
test (L.denr=0) (L.pdenw=0) (L.ndenw=0)
*Reject the null 
//TEST LR SYMMETRY
test (L.pdenw-L.ndenw=0)
*Null rejected
//TEST SR SYMMETR
test (pddenw-nddenw=0)
*we reject the null hypothesis
estat hettest, iid
*H0 is not rejected - no heteroskedasticity. 
estat bgodfrey, lags(4)
*H0 is not rejected - serial correlation :)
estat ovtest
*H0 is rejected - omitted variable. :(
estat archlm, lags(4)
*H0 is not rejected - no ARCH effects. :)
reg ddenr L.denr L.pdenw L.ndenw L(1/1).ddenr L(0/1).pdenr L(0/1).ndenw, robust

France
// asymmetric dynamic regression: you may add more lags if necessary
reg dfrar L.frar L.pfraw L.nfraw L(1/1).dfrar L(0/1).pdfraw L(0/1).ndfraw 
//PSS F TEST
test (L.frar=0) (L.pfraw=0) (L.nfraw=0)
*Reject the null
//TEST LR SYMMETRY
test (L.pfraw-L.nfraw=0)
*Reject null
//TEST SR SYMMETR
test (pdfraw-ndfraw=0)
*null rejected 
estat hettest, iid
*H0 is not rejected - no heteroskedasticity. 0
estat bgodfrey, lags(4)
*we fail to reject the null
estat ovtest
*H0 is rejected - omitted variable. 
estat archlm, lags(4)
*H0 is not rejected - no ARCH effects.. :)
reg dfrar L.frar L.pfraw L.nfraw L(1/1).dfrar L(0/1).pdfraw L(0/1).ndfraw, robust

Netherlands
// asymmetric dynamic regression: you may add more lags if necessary
reg dnetr L.netr L.pnetw L.nnetw L(1/1).dnetr L(0/1).pdnetw L(0/1).ndnetw 
//PSS F TEST
test (L.netr=0) (L.pnetw=0) (L.nnetw=0)
*we fail to reject the null hypothesis 
//TEST LR SYMMETRY
test (L.pnetw-L.nnetw=0)
*we fail to reject the null hypothesis 
//TEST SR SYMMETR
test (pdnetw-ndnetw=0)
*we fail to reject the null hypothesis 
estat hettest, iid
*H0 is rejected - heteroskedasticity. :(
estat bgodfrey, lags(4)
*H0 is not rejected - serial correlation :)
estat ovtest
*H0 is not rejected - omitted variable. :)
estat archlm, lags(4)
*H0 is not rejected - no ARCH effects
reg dnetr L.netr L.pnetw L.nnetw L(1/1).dnetr L(0/1).pdnetw L(0/1).ndnetw, robust

Switzerland
// asymmetric dynamic regression: you may add more lags if necessary
reg dswir L.swir L.pswiw L.nswiw L(1/1).dswir L(0/1).pdswiw L(0/1).ndswiw 
//PSS F TEST
test (L.swir=0) (L.pswiw=0) (L.nswiw=0)
*we rejact the null 
//TEST LR SYMMETRY
test (L.pswiw-L.nswiw=0)
*we reject the null
//TEST SR SYMMETR
test (pdswiw-ndswiw=0)
*H0 is not rejected
estat hettest, iid
*H0 is not rejected, no heteroskedasticity
estat bgodfrey, lags(4)
*H0 is not rejected -no serial correlation
estat ovtest
*H0 is rejected - omitted variable. :(
estat archlm, lags(4)
*H0 is not rejected - no ARCH effects
reg dswir L.swir L.pswiw L.nswiw L(1/1).dswir L(0/1).pdswiw L(0/1).ndswiw, robust

United Kingdom
// asymmetric dynamic regression: you may add more lags if necessary
reg dukr L.ukr L.pukw L.nukw L(1/1).dukr L(0/1).pdukw L(0/1).ndukw 
//PSS F TEST
test (L.ukr=0) (L.pukw=0) (L.nukw=0)
*We reject the null 
//TEST LR SYMMETRY
test (L.pukw-L.nukw=0)
*we fail to reject the null
//TEST SR SYMMETR
test (pdukw-ndukw=0)
*we fail to reject the null
estat hettest, iid
*H0 is rejected - heteroskedasticity. :(
estat bgodfrey, lags(4)
*H0 is rejected - no serial correlation 
estat ovtest
*H0 is rejected - omitted variable. :(
estat archlm, lags(4)
*H0 is not rejected - no ARCH effects
reg dukr L.ukr L.pukw L.nukw L(1/1).dukr L(0/1).pdukw L(0/1).ndukw , robust


Gas 2
// Static symmetric regression	-- BASIC INITIAL. CHECK FOR STATISTICAL SIGNIFICANCE
generate trend = _n
regress ukr ukw trend
*statistically significant linear relationship between the dependent variable (ukr) and the trend variable.statistically significant relationship
regress denr denw trend
*not statistically significant,cannot reject the null hypothesis that the coefficient for the "trend" variable is zero.
regress frar fraw trend
*statistically significant at the 0.05 level
regress netr netw trend
*we cannot reject the null hypothesis
regress swir swiw trend
*eject the null hypothesis and conclude that there is a significant relationship

NARDL
nardl denr denw if tin(2011q1,2019q2), p(7) q(6)
*statistically significant
nardl frar fraw if tin(2011q1,2019q2), p(7) q(6)
*statistically significance
nardl netr netw if tin(2011q1,2019q2), p(7) q(6)
*statistically significance
nardl swir swiw if tin(2011q1,2019q2), p(7) q(6)
*statistically significance
nardl ukr ukw if tin(2011q1,2019q2), p(7) q(6)
*statistically significance

constraint 1 L2._dy L3._dy L4._dy L5._dy L6._dy L7. 
constraint 2 L2._dx1p L3._dx1p L4._dx1p 
constraint 3 L2._dx1n L3._dx1n L4._dx1n 

nardl denr denw if tin(2011q1,2019q2), p(7) q(6) h(80) constraints(1/3) plot bootstrap(100) level(90)
nardl frar fraw if tin(2011q1,2019q2), p(7) q(6) h(80) constraints(1/3) plot bootstrap(100) level(90)
nardl netr netw if tin(2011q1,2019q2), p(7) q(6) h(80) constraints(1/3) plot bootstrap(100) level(90)
nardl swir swiw if tin(2011q1,2019q2), p(7) q(6) h(80) constraints(1/3) plot bootstrap(100) level(90)
nardl ukr ukw if tin(2011q1,2019q2), p(7) q(6) h(80) constraints(1/3) plot bootstrap(100) level(90)

. graph export "C:\Users\Radhika\Desktop\den.jpg", as(jpg) name("Graph") quality(100) file C:\Users\Radhika\Desktop\den.jpg written in JPEG format

. graph export "C:\Users\Radhika\Desktop\fra.jpg", as(jpg) name("Graph") quality(100)
file C:\Users\Radhika\Desktop\fra.jpg written in JPEG format

 graph export "C:\Users\Radhika\Desktop\net.jpg", as(jpg) name("Graph") quality(100) file C:\Users\Radhika\Desktop\net.jpg written in JPEG format

 graph export "C:\Users\Radhika\Desktop\Swi.jpg", as(jpg) name("Graph") quality(100) file C:\Users\Radhika\Desktop\Swi.jpg written in JPEG format

graph export "C:\Users\Radhika\Desktop\uk.jpg", as(jpg) name("Graph") quality(100) file C:\Users\Radhika\Desktop\uk.jpg written in JPEG format

log close

