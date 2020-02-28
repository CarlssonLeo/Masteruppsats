********************************************************************************
**********      			Replication file: 						************
**********				Broms, Dahlström, and Fazekas 				************
**********     		Comparative Political Studies (forthc.) 		************
**********	Political competition and public procurement outcomes	************
**********					December 7, 2018						************
********************************************************************************

/* 	Note: The data underlying figure 1 and the matching analysis in the appendix (Table A3-A8, Figure A2-A4, Table A4, Table A5, Table A6, Table A7, Table A8) cannot be published due to intellectual property rights reasons. 
	The detailed, contract-level data used in the matching analyses was obtained from the Opic database by Visma Commerce AB (support.opic@visma.com), a private sector data aggregator and publisher, on the 8th of February 2016.
*/


	*cd "bdf_replication"
use bdf_replication, clear
********************************************************************************************************************************************************************************************************************************************************************************************************************************
*** WITHIN-MUNICIPALITY VARIATION IN STABILITY
tab sd_stability if allsbmissing!=1 & year==2009
********************************************************************************************************************************************************************************************************************************************************************************************************************************
*** FIGURE 2. ONE-PARTY-RULE/NEW RULER - SINGLE BIDDING, BIVARIATE
quietly reg sb_mps onepartyrule if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	margins, at(onepartyrule=(0)) vsquish post
		est store opr0
quietly reg sb_mps onepartyrule if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 , robust
	margins, at(onepartyrule=(1)) vsquish post
		est store opr1
quietly xtreg sb i.stability, fe cluster(mc)
	margins, at(stability=(0)) vsquish post
		est store stab0		
quietly xtreg sb i.stability, fe cluster(mc)
	margins, at(stability=(1)) vsquish post
		est store stab1	
quietly xtreg sb i.stability, fe cluster(mc)
	margins, at(stability=(2)) vsquish post
		est store stab2
		
coefplot ///
	(opr1, offset(-.15) recast(bar) barwidth(0.1) fc(white) lcolor(black) lwidth(medium) ciopts(lcolor(black) recast(rcap) lwidth(medthick))) ///
	(opr0, offset(-.35) recast(bar) barwidth(0.1) fc(white) lcolor(black) lwidth(medium) ciopts(lcolor(black) recast(rcap) lwidth(medthick))) ///
	(stab0, offset(.10) recast(bar) barwidth(0.1) fc(white) lcolor(black) lwidth(medium) ciopts(lcolor(gs2) recast(rcap) lwidth(medthick))) ///
	(stab1, offset(.25) recast(bar) barwidth(0.1) fc(white) lcolor(black) lwidth(medium) ciopts(lcolor(gs2) recast(rcap) lwidth(medthick))) ///
	(stab2, offset(.40) recast(bar) barwidth(0.1) fc(white) lcolor(black) lwidth(medium) ciopts(lcolor(gs2) recast(rcap) lwidth(medthick))), ///
		vertical plotregion(style(none) m(l=0 r=0 t=0 b=0)) graphregion(m(tiny)) citop msize(large) ///
		legend(off) ///
		xline(1, lp(dash)) xscale(noline) xtitle("One-party rule				Stability", size(medlarge)) xlabel(.65 "No" .85 "Yes" 1.1 "New" 1.25 `""Reelected" "once""' 1.4 `""Reelected" "twice" "or more""', tlc(white) labsize(medsmall)) ///
		ylabel(0(2.5)15, tlc(white) labs(medium) grid glcolor(gs10)) yscale(r(0 16) noline) ytitle("Single bidding (%)", height(5) size(medlarge)) ///
		name(figure2, replace) xsize(4) ysize(3)
			*graph export figure2.png, replace
			
********************************************************************************************************************************************************************************************************************************************************************************************************************************
*** TABLE 1. ONE-PARTY RULE
reg sb_mps onepartyrule if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using table2, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, NO) ///
	**addnote("Dependent variable: Single bidding ratio. Data averaged for the 2011-14 term period. Robust standard errors in parentheses; *** p<0.01 ** p<0.05 * p<0.1") replace 
reg sb_mps onepartyrule pop6dum2-pop6dum6 if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using table2, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, NO) append 
reg sb_mps onepartyrule logarea if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using table2, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, NO) append 
reg sb_mps onepartyrule scb_medinc_mps if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using table2, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, NO) append 
reg sb_mps onepartyrule i.mayoralparty if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using table2, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, YES) append 
reg sb_mps onepartyrule i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using table2, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, YES, Party FE, NO) append 
reg sb_mps onepartyrule pop6dum2-pop6dum6 logarea scb_medinc_mps i.mayoralparty i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using table2, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, YES, Party FE, YES) append 
	
********************************************************************************************************************************************************************************************************************************************************************************************************************************
*** TABLE 3. STABILITY 
xtreg sb stabdum2 stabdum3, fe robust
	*outreg2 using table3, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, No", "Party FEs:, No", "County FEs:, No") cttop("Fixed effects") ///
	*	*addnote("Dependent variable: Single bidding ratio. Robust standard errors in parentheses; *** p<0.01 ** p<0.05 * p<0.1). GMM models use the twostep estimator with Windmeijer correction; Vote share, ruling party & LDV treated as predetermined and instrumented GMM-style (lag depth 1-4). Other covariates treated as exogenous and instrumented IV-style.") replace
xtreg sb stabdum2 stabdum3 scb_medinc , fe robust
	*outreg2 using table3, excel label dec(2) ///
		*drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, No", "Party FEs:, No", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtreg sb stabdum2 stabdum3 i.mayoralparty , fe robust
	*outreg2 using table3, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, No", "Party FEs:, Yes", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtreg sb stabdum2 stabdum3 i.year, fe robust
	*outreg2 using table3, excel label dec(2) ///
	**	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, Yes", "Party FEs:, No", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append	
xtreg sb stabdum2 stabdum3 scb_medinc i.mayoralparty i.year, fe robust
	*outreg2 using table3, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtreg sb ldv stabdum2 stabdum3 scb_medinc i.mayoralparty i.year, fe robust
	*outreg2 using table3, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtabond2 sb stabdum2 stabdum3 ldv logarea scb_medinc pop6dum2-pop6dum6 i.mayoralparty i.reg i.year, ///
 gmm(ldv stabdum2 stabdum3, lag(. 4)) iv(logarea scb_medinc pop6dum2-pop6dum6 i.mayoralparty i.reg i.year) two robust artest(5)
	*outreg2 using table3, excel label dec(2) ///
	*	addstat("No. municipalities", e(N_g), "AR1 (p)", e(ar1p), "AR2 (p)", e(ar2p), "Hansen J statistic (p)", e(hansenp), "No. Instruments", e(j)) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, No", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, Yes") noni cttop("Sys-GMM") append							
xtabond2 sb stabdum2 stabdum3 ldv scb_medinc i.mayoralparty i.year , ///
 gmm(ldv stabdum2 stabdum3, lag(. 4)) iv(scb_medinc i.mayoralparty i.year) two robust nolevel artest(5)
	*outreg2 using table3, excel label dec(2) ///
	*	addstat("No. municipalities", e(N_g), "AR1 (p)", e(ar1p), "AR2 (p)", e(ar2p), "Hansen J statistic (p)", e(hansenp), "No. Instruments", e(j)) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, No", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, NO") noni cttop("Diff-GMM") append		
		
********************************************************************************************************************************************************************************************************************************************************************************************************************************	
*** FIGURE 3. MECHANISMS 
* PANEL A. GOOD RELATIONSHIP BETWEEN MAJORITY & OPPOSITION
quietly reg kfu_majoppfriends onepartyrule if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=., robust
	quietly margins, dydx(onepartyrule) atmeans post
	est store gfop1
quietly reg kfu_majoppfriends onepartyrule i.pop6cat logarea scb_medinc_mps i.mayoralparty i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=. , robust
	quietly margins, dydx(onepartyrule) at(pop6cat=4 mayoralparty=1 reg=14) atmeans post
	est store gfop2
quietly reg sb_mps kfu_majoppfriends if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=., robust
	quietly margins, dydx(kfu_majoppfriends) atmeans post
	est store sbgf1
quietly reg sb_mps kfu_majoppfriends i.pop6cat logarea scb_medinc_mps i.mayoralparty i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=. , robust
	quietly margins, dydx(kfu_majoppfriends) at(pop6cat=4 mayoralparty=1 reg=14) atmeans post
	est store sbgf2

quietly coefplot ///
	(gfop1, m(S) mc(white) offset(-.15) yaxis(1) yscale(r(-1.3 1.3) noline axis(1)) ylabel(-1(.5)1, grid glcolor(gs10) tlc(white) labs(medium) axis(1)) yline(0) ) ///
	(gfop2, m(S) mc(black) offset(.15) yaxis(2) yscale(r(-1.3 1.3) noline axis(2)) ylabel(-1(.5)1, nogrid glcolor(gs10) tlc(white) labs(medium) axis(2) labc(white) ) yline(0)) ///
	, recast(scatter) keep(onepartyrule) vertical plotregion(style(none) m(l=0 r=0 t=0 b=0)) graphregion(m(tiny)) title("") ///
	mlc(black) msize(vlarge) lp(solid) lwidth(thick)ciopt(lcolor(black) recast(rcap) lwidth(medthick)) ////
	 ///
	ytitle("Marginal effects of" "{it:One-party rule}", height(8) size(vlarge) axis(1)) ///
	ytitle("Marginal effects of" "{it:Good majority/opposition relation}", height(7) c(white) size(vlarge) axis(2)) ///
	xscale( noline) ///
	title("DV: Good majority/opposition relation", size(vlarge)) ///
	xlabel(, nolab notick) ///
	legend(off region(lstyle(none) m(l=0 r=0 t=0 b=0)) ///
	order(2 4) label(2 "Bivariate") label(4 "All controls") size(medlarge)) ///
	name(figure3x1, replace) xsize(1.5) ysize(1) yline(0, lc(black) lw(*1.5))

quietly coefplot ///
	(sbgf1, m(S) mc(white) offset(-.15) yaxis(1) yscale(r(-1.3 1.3) noline axis(1)) ylabel(-1(.5)1, grid glcolor(gs10) tlc(white) labs(medium) axis(1)) yline(0) ) ///
	(sbgf2, m(S) mc(black) offset(.15) yaxis(2) yscale(r(-1.3 1.3) noline axis(2)) ylabel(-1(.5)1, nogrid glcolor(gs10) tlc(white) labs(medium) axis(2) labc(white) ) yline(0)) ///
	, recast(scatter) keep(kfu_majoppfriends) vertical plotregion(style(none) m(l=0 r=0 t=0 b=0)) graphregion(m(tiny)) title("") ///
	mlc(black) msize(vlarge) lp(solid) lwidth(thick)ciopt(lcolor(black) recast(rcap) lwidth(medthick)) ////
	 ///
	ytitle("Marginal effects of" "{it:Good majority/opposition relation}", height(8) size(vlarge) axis(1)) ///
	ytitle("Marginal effects of" "{it:Good majority/opposition relation}", height(7) c(white) size(vlarge) axis(2)) ///
	xscale( noline) ///
	title("DV: Single bidding", size(vlarge)) ///
	xlabel(, nolab notick) ///
	legend(region(lstyle(none) m(l=0 r=0 t=0 b=0)) ///
	order(2 4) label(2 "Bivariate") label(4 "All controls") size(medlarge)) ///
	name(figure3x2, replace) xsize(1.5) ysize(1) yline(0, lc(black) lw(*1.5))
	
	gr combine figure3x1 figure3x2, col(1) title("Panel A", size(huge)) xsize(1.5) ysize(3) name(figure3x, replace) 

	* PANEL B. AUDIT CHAIRED BY MAJORITY
quietly logit audmaj_full onepartyrule if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=., robust or
	quietly margins, dydx(onepartyrule) atmeans post
	est store amop1
quietly logit audmaj_full onepartyrule i.pop6cat logarea scb_medinc_mps i.mayoralparty_aud i.nuts1 if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=., robust or
	quietly margins, dydx(onepartyrule) at(pop6cat=4 mayoralparty_aud=1 nuts1=2) atmeans post
	est store amop2
quietly reg sb_mps audmaj_full if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 , robust
	quietly margins, dydx(audmaj_full) atmeans post
	est store sbam1
quietly reg sb_mps audmaj_full i.pop6cat logarea scb_medinc_mps i.mayoralparty_aud i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 , robust
	quietly margins, dydx(audmaj_full) at(pop6cat=4 mayoralparty_aud=1 reg=14) atmeans post
	est store sbam2

quietly coefplot ///
	(amop1, m(S) mc(white) offset(-.15) yaxis(1) yscale(r(-.5 .5) noline axis(1)) ylabel(-.4(.2).4, grid glcolor(gs10) tlc(white) labs(medium) axis(1)) yline(0) ) ///
	(amop2, m(S) mc(black) offset(.15) yaxis(2) yscale(r(-.5 .5) noline axis(2)) ylabel(-.4(.2).4, nogrid glcolor(gs10) tlc(white) labs(medium) axis(2) labc(white) ) yline(0)) ///
	, recast(scatter) keep(onepartyrule) vertical plotregion(style(none) m(l=0 r=0 t=0 b=0)) graphregion(m(tiny)) title("") ///
	mlc(black) msize(vlarge) lp(solid) lwidth(thick)ciopt(lcolor(black) recast(rcap) lwidth(medthick)) ////
	///
	ytitle("Marginal effects of" "{it:One-party rule}", height(8) size(vlarge) axis(1)) ///
	ytitle("Marginal effects of" "{it:Audit chair from majority}", height(7) c(white) size(vlarge) axis(2)) ///
	xscale( noline) ///
	title("DV: Audit chair from majority", size(vlarge)) ///
	xlabel(, nolab notick) ///
	legend(off region(lstyle(none) m(l=0 r=0 t=0 b=0)) ///
	order(2 4) label(2 "Bivariate") label(4 "All controls") size(medlarge)) ///
	name(figure3a1, replace) xsize(1.5) ysize(1) yline(0, lc(black) lw(*1.5))

quietly coefplot ///
	(sbam1, m(S) mc(white) offset(-.15) yaxis(1) yscale(r(-4.2 4.2) noline axis(1)) ylabel(-4(2)4, grid glcolor(gs10) tlc(white) labs(medium) axis(1)) yline(0) ) ///
	(sbam2, m(S) mc(black) offset(.15) yaxis(2) yscale(r(-4.2 4.2) noline axis(2)) ylabel(-4(2)4, nogrid glcolor(gs10) tlc(white) labs(medium) axis(2) labc(white) ) yline(0)) ///
	, recast(scatter) keep(audmaj_full) vertical plotregion(style(none) m(l=0 r=0 t=0 b=0)) graphregion(m(tiny)) title("") ///
	mlc(black) msize(vlarge) lp(solid) lwidth(thick)ciopt(lcolor(black) recast(rcap) lwidth(medthick)) ////
	///
	ytitle("Marginal effects of" "{it:Audit chair from majority}", height(8) size(vlarge) axis(1)) ///
	ytitle("Marginal effects of" "{it:Audit chair from majority}", height(7) c(white) size(vlarge) axis(2)) ///
	xscale( noline) ///
	title("DV: Single bidding", size(vlarge)) ///
	xlabel(, nolab notick) ///
	legend(region(lstyle(none) m(l=0 r=0 t=0 b=0)) ///
	order(2 4) label(2 "Bivariate") label(4 "All controls") size(medlarge)) ///
	name(figure3a2, replace) xsize(1.5) ysize(1) yline(0, lc(black) lw(*1.5))
	
	gr combine figure3a1 figure3a2, col(1) title("Panel B", size(huge)) xsize(1.5) ysize(3) name(figure3a, replace) 

		* PANEL C. MEDIA DRIVES ELECTION PROMISES
quietly reg mediapromise_maj onepartyrule if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=., robust
	quietly margins, dydx(onepartyrule) atmeans post
	est store mpop1
quietly reg mediapromise_maj onepartyrule i.pop6cat logarea scb_medinc_mps i.mayoralparty i.reg coverage if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=. , robust
	quietly margins, dydx(onepartyrule) at(pop6cat=4 mayoralparty=1 reg=14) atmeans post
	est store mpop2
quietly reg sb_mps mediapromise_maj if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=., robust
	quietly margins, dydx(mediapromise_maj) atmeans post
	est store sbmp1
quietly reg sb_mps mediapromise_maj i.pop6cat logarea scb_medinc_mps i.mayoralparty i.reg coverage if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=. , robust
	quietly margins, dydx(mediapromise_maj) at(pop6cat=4 mayoralparty=1 reg=14) atmeans post
	est store sbmp2

quietly coefplot ///
	(mpop1, m(S) mc(white) offset(-.15) yaxis(1) yscale(r(-1.2 1.2) noline axis(1)) ylabel(-1(.5)1, grid glcolor(gs10) tlc(white) labs(medium) axis(1)) yline(0) ) ///
	(mpop2, m(S) mc(black) offset(.15) yaxis(2) yscale(r(-1.2 1.2) noline axis(2)) ylabel(-1(.5)1, nogrid glcolor(gs10) tlc(white) labs(medium) axis(2) labc(white)) yline(0)) ///
	, recast(scatter) keep(onepartyrule) vertical plotregion(style(none) m(l=0 r=0 t=0 b=0)) graphregion(m(tiny)) title("") ///
	mlc(black) msize(vlarge) lp(solid) lwidth(thick)ciopt(lcolor(black) recast(rcap) lwidth(medthick)) ////
	 ///
	ytitle("Marginal effects of" "{it:One-party rule}", height(8) size(vlarge) axis(1)) ///
	ytitle("Marginal effects of" "{it:One-party rule}", height(7) c(white) size(vlarge) axis(2)) ///
	xscale( noline) ///
	title("DV: Media influence", size(vlarge)) ///
	xlabel(, nolab notick) ///
	legend(off region(lstyle(none) m(l=0 r=0 t=5 b=0)) ///
	order(2 4) label(2 "Bivariate") label(4 "All controls") size(medlarge)) ///
	name(figure3b1, replace) xsize(1.5) ysize(1) yline(0, lc(black) lw(*1.5))

	quietly coefplot ///
	(sbmp1, m(S) mc(white) offset(-.15) yaxis(1) yscale(r(-2.5 2.5) noline axis(1)) ylabel(-2.5(1.25)2.5, grid glcolor(gs10) tlc(white) labs(medium) axis(1)) yline(0) ) ///
	(sbmp2, m(S) mc(black) offset(.15) yaxis(2) yscale(r(-2.5 2.5) noline axis(2)) ylabel(-2.5(1.25)2.5, nogrid glcolor(gs10) tlc(white) labs(medium) axis(2) labc(white)) yline(0)) ///
	, recast(scatter) keep(mediapromise_maj) vertical plotregion(style(none) m(l=0 r=0 t=0 b=0)) graphregion(m(tiny)) title("") ///
	mlc(black) msize(vlarge) lp(solid) lwidth(thick)ciopt(lcolor(black) recast(rcap) lwidth(medthick)) ////
	 ///
	ytitle("Marginal effects of" "{it:Media influence}", height(8) size(vlarge) axis(1)) ///
	ytitle("Marginal effects of" "{it:Media influence}", height(7) c(white) size(vlarge) axis(2)) ///
	xscale( noline) ///
	title("DV: Single bidding", size(vlarge)) ///
	xlabel(, nolab notick) ///
	legend(region(lstyle(none) m(l=0 r=0 t=5 b=0)) ///
	order(2 4) label(2 "Bivariate") label(4 "All controls") size(medlarge)) ///
	name(figure3b2, replace) xsize(1.5) ysize(1) yline(0, lc(black) lw(*1.5))
	
	gr combine figure3b1 figure3b2, col(1) title("Panel C", size(huge)) xsize(1.5) ysize(3) name(figure3b, replace)

		* PANEL D. HUMAN CAPITAL IN MUNICIPALITY
quietly reg kol_empedu_mps onepartyrule if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=., robust
	quietly margins, dydx(onepartyrule) atmeans post
	est store hcop1
quietly reg kol_empedu_mps onepartyrule i.pop6cat logarea scb_medinc_mps i.mayoralparty i.reg scb_highered_mps kol_kop_egentlig_mps if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=. , robust
	quietly margins, dydx(onepartyrule) at(pop6cat=4 mayoralparty=1 reg=14) atmeans post
	est store hcop2
quietly reg sb_mps kol_empedu_mps if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=., robust
	quietly margins, dydx(kol_empedu_mps) atmeans post
	est store sbhc1
quietly reg sb_mps kol_empedu_mps i.pop6cat logarea scb_medinc_mps i.mayoralparty i.reg scb_highered_mps kol_kop_egentlig_mps if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=., robust
	quietly margins, dydx(kol_empedu_mps) at(pop6cat=4 mayoralparty=1 reg=14) atmeans post
	est store sbhc2

quietly coefplot ///
	(hcop1, m(S) mc(white) offset(-.15) yaxis(1) yscale(r(-6 6) noline axis(1)) ylabel(-5(2.5)5, grid glcolor(gs10) tlc(white) labs(medium) axis(1)) yline(0)) ///
	(hcop2, m(S) mc(black) offset(.15) yaxis(2) yscale(r(-6 6) noline axis(2)) ylabel(-5(2.5)5, grid glcolor(gs10) tlc(white) labs(medium) axis(2) labc(white)) yline(0)) ///
	, recast(scatter) keep(onepartyrule) vertical plotregion(style(none) m(l=0 r=0 t=0 b=0)) graphregion(m(tiny)) title("") ///
	mlc(black) msize(vlarge) lp(solid) lwidth(thick)ciopt(lcolor(black) recast(rcap) lwidth(medthick)) ////
	 ///
	ytitle("Marginal effects of" "{it:One-party rule}", height(8) size(vlarge) axis(1)) ///
	ytitle("Marginal effects of" "{it:One-party rule}", height(7) c(white) size(vlarge) axis(2)) ///
	xscale( noline) ///
	title("DV: Bureaucratic human capital", size(vlarge)) ///
	xlabel(, nolab notick) ///
	legend(off region(lstyle(none) m(l=0 r=0 t=0 b=0)) ///
	order(2 4) label(2 "Bivariate") label(4 "All controls") size(medlarge)) ///
	name(figure3c1, replace) xsize(1.5) ysize(1) yline(0, lc(black) lw(*1.5))

quietly coefplot ///
	(sbhc1, m(S) mc(white) offset(-.15) yaxis(1) yscale(r(-.75 .75) noline axis(1)) ylabel(-.75(.25).75, grid glcolor(gs10) tlc(white) labs(medium) axis(1)) yline(0) ) ///
	(sbhc2, m(S) mc(black) offset(.15) yaxis(2) yscale(r(-.75 .75) noline axis(2)) ylabel(-.75(.25).75, nogrid glcolor(gs10) tlc(white) labs(medium) axis(2) labc(white)) yline(0)) ///
	, recast(scatter) keep(kol_empedu_mps) vertical plotregion(style(none) m(l=0 r=0 t=0 b=0)) graphregion(m(tiny)) title("") ///
	mlc(black) msize(vlarge) lp(solid) lwidth(thick)ciopt(lcolor(black) recast(rcap) lwidth(medthick)) ////
	 ///
	ytitle("Marginal effects of" "{it:Human capital}", height(8) size(vlarge) axis(1)) ///
	ytitle("Marginal effects of" "{it:Human capital}", height(7) c(white) size(vlarge) axis(2)) ///
	xscale( noline) ///
	title("DV: Single bidding", size(vlarge)) ///
	xlabel(, nolab notick) ///
	legend(region(lstyle(none) m(l=0 r=0 t=0 b=0)) ///
	order(2 4) label(2 "Bivariate") label(4 "All controls") size(medlarge)) ///
	name(figure3c2, replace) xsize(1.5) ysize(1) yline(0, lc(black) lw(*1.5))
	
	gr combine figure3c1 figure3c2, col(1) title("Panel D", size(huge)) xsize(1.5) ysize(3) name(figure3c, replace)

		* PANEL E. LOCAL WINNER
quietly reg localwin_town_mps onepartyrule if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & contracts_lw_mis_mps>2, robust
	quietly margins, dydx(onepartyrule) atmeans post
	est store lwop1
quietly reg localwin_town_mps onepartyrule i.pop6cat logarea scb_medinc_mps i.mayoralparty i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & contracts_lw_mis_mps>2 , robust
	quietly margins, dydx(onepartyrule) at(pop6cat=4 mayoralparty=1 reg=14) atmeans post
	est store lwop2
quietly reg sb_mps localwin_town_mps if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & contracts_lw_mis_mps>2, robust
	quietly margins, dydx(localwin_town_mps) atmeans post
	est store sblw1
quietly reg sb_mps localwin_town_mps i.pop6cat logarea scb_medinc_mps i.mayoralparty i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & contracts_lw_mis_mps>2, robust
	quietly margins, dydx(localwin_town_mps) at(pop6cat=4 mayoralparty=1 reg=14) atmeans post
	est store sblw2

quietly coefplot ///
	(lwop1, m(S) mc(white) offset(-.15) yaxis(1) yscale(r(-6 6) noline axis(1)) ylabel(-5(2.5)5, grid glcolor(gs10) tlc(white) labs(medium) axis(1)) yline(0) ) ///
	(lwop2, m(S) mc(black) offset(.15) yaxis(2) yscale(r(-6 6) noline axis(2)) ylabel(-5(2.5)5, grid glcolor(gs10) tlc(white) labs(medium) axis(2) labc(white)) yline(0)) ///
	, recast(scatter) keep(onepartyrule) vertical plotregion(style(none) m(l=0 r=0 t=0 b=0)) graphregion(m(tiny)) title("") ///
	mlc(black) msize(vlarge) lp(solid) lwidth(thick)ciopt(lcolor(black) recast(rcap) lwidth(medthick)) ////
	 ///
	ytitle("Marginal effects of" "{it:One-party rule}", height(8) size(vlarge) axis(1)) ///
	ytitle("Marginal effects of" "{it:Local winner}", height(7) c(white) size(vlarge) axis(2)) ///
	xscale( noline) ///
	title("DV:Local winner", size(vlarge)) ///
	xlabel(, nolab notick) ///
	legend(off region(lstyle(none) m(l=0 r=0 t=0 b=0)) ///
	order(2 4) label(2 "Bivariate") label(4 "All controls") size(medlarge)) ///
	name(figure3d1, replace) xsize(1.5) ysize(1) yline(0, lc(black) lw(*1.5))

quietly coefplot ///
	(sblw1, m(S) mc(white) offset(-.15) yaxis(1) yscale(r(-.25 .25) noline axis(1)) ylabel(-.25(.125).25, grid glcolor(gs10) tlc(white) labs(medium) axis(1)) yline(0) ) ///
	(sblw2, m(S) mc(black) offset(.15) yaxis(2) yscale(r(-.25 .25) noline axis(2)) ylabel(-.25(.125).25, nogrid glcolor(gs10) tlc(white) labs(medium) axis(2) labc(white)) yline(0)) ///
	, recast(scatter) keep(localwin_town_mps) vertical plotregion(style(none) m(l=0 r=0 t=0 b=0)) graphregion(m(tiny)) title("") ///
	mlc(black) msize(vlarge) lp(solid) lwidth(thick)ciopt(lcolor(black) recast(rcap) lwidth(medthick)) ////
	 ///
	ytitle("Marginal effects of" "{it:Local winner}", height(8) size(vlarge) axis(1)) ///
	ytitle("Marginal effects of" "{it:Local winner}", height(7) c(white) size(vlarge) axis(2)) ///
	xscale( noline) ///
	title("DV: Single bidding", size(vlarge)) ///
	xlabel(, nolab notick) ///
	legend(region(lstyle(none) m(l=0 r=0 t=0 b=0)) ///
	order(2 4) label(2 "Bivariate") label(4 "All controls") size(medlarge)) ///
	name(figure3d2, replace) xsize(1.5) ysize(1) yline(0, lc(black) lw(*1.5))
	
	gr combine figure3d1 figure3d2, col(1) title("Panel E", size(huge)) xsize(1.5) ysize(3) name(figure3d, replace)

	* FIGURE 3
	gr combine figure3x figure3a figure3b figure3c figure3d, rows(1) title("") iscale(.6) xsize(5) ysize(2) graphregion(m(tiny)) name(figure3, replace)
			*graph export figure3.png, replace
********************************************************************************************************************************************************************************************************************************************************************************************************************************
********** ROBUSTNESS
********************************************************************************************************************************************************************************************************************************************************************************************************************************
*** FIGURE A1. OUTLIERS IN CROSS-SECTIONAL DATA
tw sc sb_mps onepartyrule if year==2012 & m!="Dals-Ed" & contracts_sb_mps>2, m(+) || ///
	sc sb_mps onepartyrule if year==2012 & contracts_sb_mps<=2, jitter(9) m(+) mc(gs0) msize(large) || ///
	sc sb_mps onepartyrule if m=="Dals-Ed" & year==2012, ///
	ylabel(, grid notick) xlabel(0 "No" 1 "Yes", grid notick) yscale(noline) xscale(noline) plotregion(style(none) m(l=4 r=4 t=4 b=4)) graphregion(m(tiny)) title("") ///
	legend(label(1 ">2 contracts") label(2 "<=2 contracts") label(3 "Dals-Ed") region(lstyle(none) m(l=-11 r=-11 t=0 b=0)) symx(*.5)) xsize(1) ysize(1.5) name(figurea1, replace)
		*graph export figure1.png, replace
********************************************************************************************************************************************************************************************************************************************************************************************************************************
*** TABLE A2. DESCRIPTIVES
		drop _est*
	order sb_mps sb onepartyrule stabdum* pop6dum* logarea scb_medinc_mps scb_medinc mayoralpartydum* regdum*
	* outreg2 using tablea2 if sb_mps!=. & m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, excel label dec(2) sum(log) keep(sb_mps onepartyrule stab12dum* pop6dum* logarea scb_medinc_mps mayoralpartydum* regdum* audmaj_full mediapromise_maj coverage kol_empedu_mps scb_highered_mps localwin_town_mps nobids_mps) ///
	* replace
	*outreg2 using tablea2 if sb!=., excel label dec(2) sum(log) keep(sb onepartyrule stabdum* pop6dum* logarea scb_medinc mayoralpartydum* regdum* vs_mayoralparty nosingle nobids) ///
	* append
	
******************************************************************************************************************************************************************************************************************************************************************************************************************************** 
*** TABLE A9. BETWEEN & POLS MODELS FOR ONE-PARTY RULE
	* BETWEEN ESTIMATOR
xtreg sb onepartyrule i.year, be
	*outreg2 using tablea9, excel label sortvar(onepartyrule) drop(i.mc i.mayoralparty i.reg i.year) dec(2) addtext(County FE, NO, Year FE, YES, Party FE, NO) noni addstat("No. municipalities", e(N_g)) ///
	*cttop("Between estimator") ///
	*addnote("Dependent variable: Single bidding ratio. Standard errors in parentheses (clustered by municipality in POLS estimations). *** p<0.01 ** p<0.05 * p<0.1)") replace
xtreg sb onepartyrule pop6dum2-pop6dum6 i.year, be
	*outreg2 using tablea9, excel label sortvar(onepartyrule) drop(i.mc i.mayoralparty i.reg i.year) dec(2) addtext(County FE, NO, Year FE, YES, Party FE, NO) noni addstat("No. municipalities", e(N_g)) nonotes append 
xtreg sb onepartyrule logarea i.year, be
	*outreg2 using tablea9, excel label sortvar(onepartyrule) drop(i.mc i.mayoralparty i.reg i.year) dec(2) addtext(County FE, NO, Year FE, YES, Party FE, NO) noni addstat("No. municipalities", e(N_g)) nonotes append 
xtreg sb onepartyrule scb_medinc i.year, be
	*outreg2 using tablea9, excel label sortvar(onepartyrule) drop(i.mc i.mayoralparty i.reg i.year) dec(2) addtext(County FE, NO, Year FE, YES, Party FE, NO) noni addstat("No. municipalities", e(N_g)) nonotes append 
xtreg sb onepartyrule i.mayoralparty i.year, be
	*outreg2 using tablea9, excel label sortvar(onepartyrule) drop(i.mc i.mayoralparty i.reg i.year) dec(2) addtext(County FE, NO, Year FE, YES, Party FE, YES) noni addstat("No. municipalities", e(N_g)) nonotes append 
xtreg sb onepartyrule i.reg i.year, be
	*outreg2 using tablea9, excel label sortvar(onepartyrule) drop(i.mc i.mayoralparty i.reg i.year) dec(2) addtext(County FE, YES, Year FE, YES, Party FE, NO) noni addstat("No. municipalities", e(N_g)) nonotes append 
xtreg sb onepartyrule pop6dum2-pop6dum6 logarea scb_medinc i.mayoralparty i.reg i.year, be
	*outreg2 using tablea9, excel label sortvar(onepartyrule) drop(i.mc i.mayoralparty i.reg i.year) dec(2) addtext(County FE, YES, Year FE, YES, Party FE, YES) noni addstat("No. municipalities", e(N_g)) nonotes append 
	* POLS	
reg sb onepartyrule i.year, cluster(mc)
	*outreg2 using tablea9, excel label sortvar(onepartyrule) drop(i.mc i.mayoralparty i.reg i.year) dec(2) addtext(County FE, NO, Year FE, YES, Party FE, NO) noni addstat("No. municipalities", e(N_clust)) nonotes append cttop("Pooled OLS")
reg sb onepartyrule pop6dum2-pop6dum6 i.year, cluster(mc)
	*outreg2 using tablea9, excel label sortvar(onepartyrule) drop(i.mc i.mayoralparty i.reg i.year) dec(2) addtext(County FE, NO, Year FE, YES, Party FE, NO) noni addstat("No. municipalities", e(N_clust)) nonotes append 
reg sb onepartyrule logarea i.year, cluster(mc)
	*outreg2 using tablea9, excel label sortvar(onepartyrule) drop(i.mc i.mayoralparty i.reg i.year) dec(2) addtext(County FE, NO, Year FE, YES, Party FE, NO) noni addstat("No. municipalities", e(N_clust)) nonotes append 
reg sb onepartyrule scb_medinc i.year, cluster(mc)
	*outreg2 using tablea9, excel label sortvar(onepartyrule) drop(i.mc i.mayoralparty i.reg i.year) dec(2) addtext(County FE, NO, Year FE, YES, Party FE, NO) noni addstat("No. municipalities", e(N_clust)) nonotes append 
reg sb onepartyrule i.mayoralparty i.year, cluster(mc)
	*outreg2 using tablea9, excel label sortvar(onepartyrule) drop(i.mc i.mayoralparty i.reg i.year) dec(2) addtext(County FE, NO, Year FE, YES, Party FE, YES) noni addstat("No. municipalities", e(N_clust)) nonotes append 
reg sb onepartyrule i.reg i.year, cluster(mc)
	*outreg2 using tablea9, excel label sortvar(onepartyrule) drop(i.mc i.mayoralparty i.reg i.year) dec(2) addtext(County FE, YES, Year FE, YES, Party FE, NO) noni addstat("No. municipalities", e(N_clust)) nonotes append 
reg sb onepartyrule pop6dum2-pop6dum6 logarea scb_medinc i.mayoralparty i.reg i.year, cluster(mc)
	*outreg2 using tablea9, excel label sortvar(onepartyrule) drop(i.mc i.mayoralparty i.reg i.year) dec(2) addtext(County FE, YES, Year FE, YES, Party FE, YES) noni addstat("No. municipalities", e(N_clust)) nonotes append
********************************************************************************************************************************************************************************************************************************************************************************************************************************
*** TABLE A10. STABILITY (CROSS-SECTION)
reg sb_mps i.stability12 if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea10, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, NO) ///
	*addnote("Dependent variable: Single bidding ratio. Data averaged for the 2011-14 term period. Robust standard errors in parentheses; *** p<0.01 ** p<0.05 * p<0.1") replace 
reg sb_mps i.stability12 pop6dum2-pop6dum6 if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea10, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, NO) append 
reg sb_mps i.stability12 logarea if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea10, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, NO) append 
reg sb_mps i.stability12 scb_medinc_mps if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea10, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, NO) append 
reg sb_mps i.stability12 i.mayoralparty if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea10, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, YES) append 
reg sb_mps i.stability12 i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea10, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, YES, Party FE, NO) append 
reg sb_mps i.stability12 pop6dum2-pop6dum6 logarea scb_medinc_mps i.mayoralparty i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea10, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, YES, Party FE, YES) append 
********************************************************************************************************************************************************************************************************************************************************************************************************************************
*** TABLE A11. VOTE SHARE, RULING PARTY 
xtreg sb vs_mayoralparty, fe robust
	*outreg2 using tablea11, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, No", "Party FEs:, No", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) cttop("Fixed effects") ///
		*addnote("Dependent variable: Single bidding ratio. Robust standard errors in parentheses; *** p<0.01 ** p<0.05 * p<0.1). GMM models use the twostep estimator with Windmeijer correction; Vote share, ruling party & LDV treated as predetermined and instrumented GMM-style (lag depth 1-4). Other covariates treated as exogenous and instrumented IV-style") replace
xtreg sb vs_mayoralparty scb_medinc , fe robust
	*outreg2 using tablea11, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, No", "Party FEs:, No", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtreg sb vs_mayoralparty i.mayoralparty , fe robust
	*outreg2 using tablea11, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, No", "Party FEs:, Yes", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtreg sb vs_mayoralparty i.year, fe robust
	*outreg2 using tablea11, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, Yes", "Party FEs:, No", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append	
xtreg sb vs_mayoralparty scb_medinc i.mayoralparty i.year, fe robust
	*outreg2 using tablea11, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtreg sb ldv vs_mayoralparty scb_medinc i.mayoralparty i.year, fe robust
	*outreg2 using tablea11, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtabond2 sb vs_mayoralparty ldv logarea scb_medinc pop6dum2-pop6dum6 i.mayoralparty i.reg i.year, ///
 gmm(ldv vs_mayoralparty, lag(. 4)) iv(logarea scb_medinc pop6dum2-pop6dum6 i.mayoralparty i.reg i.year) two robust artest(5)
	*outreg2 using tablea11, excel label dec(2) ///
	*	addstat("No. municipalities", e(N_g), "AR1 (p)", e(ar1p), "AR2 (p)", e(ar2p), "Hansen J statistic (p)", e(hansenp), "No. Instruments", e(j)) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, No", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, Yes") noni cttop("Sys-GMM") append							
xtabond2 sb vs_mayoralparty ldv scb_medinc i.mayoralparty i.year , ///
 gmm(ldv vs_mayoralparty, lag(. 4)) iv(scb_medinc i.mayoralparty i.year) two robust nolevel artest(5)
	*outreg2 using tablea11, excel label dec(2) ///
	*	addstat("No. municipalities", e(N_g), "AR1 (p)", e(ar1p), "AR2 (p)", e(ar2p), "Hansen J statistic (p)", e(hansenp), "No. Instruments", e(j)) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, No", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, No") noni cttop("Diff-GMM") append				
********************************************************************************************************************************************************************************************************************************************************************************************************************************
*** TABLE A12. NO SINGLE BIDDING 
logit nosingle onepartyrule, or cluster(mc)
	*outreg2 using tablea12, excel label sortvar(onepartyrule stabdum2 stabdum3) drop(i.mc i.mayoralparty i.year ) dec(2) addtext(Municipal FE, NO, Year FE, NO, Party FE, NO) cttop(Pooled logit) addstat("No. municipalities", e(N_clust)) noni eform ///
		*addnote("Dependent variable: No single bidding during year. Odds ratios displayed. Standard errors in parentheses (clustered by municipality in pooled logit estimations); *** p<0.01 ** p<0.05 * p<0.1). FE logit models estimated using the xtlogit, fe command in STATA 14.2. Number of contracts and its squared term included to account for the mechanical decrease in likelihood of receiving no single bid as number of contracts increase.") replace
logit nosingle onepartyrule scb_medinc, or cluster(mc)
	*outreg2 using tablea12, excel label sortvar(onepartyrule stabdum2 stabdum3) drop(i.mc i.mayoralparty i.year ) dec(2) addtext(Municipal FE, NO, Year FE, NO, Party FE, NO) addstat("No. municipalities", e(N_clust)) noni eform append 
logit nosingle onepartyrule i.mayoralparty, or cluster(mc)
	*outreg2 using tablea12, excel label sortvar(onepartyrule stabdum2 stabdum3) drop(i.mc i.mayoralparty i.year ) dec(2) addtext(Municipal FE, NO, Year FE, NO, Party FE, YES) addstat("No. municipalities", e(N_clust)) noni eform append 
logit nosingle onepartyrule pop6dum2-pop6dum5, or cluster(mc)
	*outreg2 using tablea12, excel label sortvar(onepartyrule stabdum2 stabdum3) drop(i.mc i.mayoralparty i.year ) dec(2) addtext(Municipal FE, NO, Year FE, NO, Party FE, NO) addstat("No. municipalities", e(N_clust)) noni eform append 
logit nosingle onepartyrule ncmc_sb_yr ncmc_sb_yr2, or cluster(mc)
	*outreg2 using tablea12, excel label sortvar(onepartyrule stabdum2 stabdum3) drop(i.mc i.mayoralparty i.year ) dec(2) addtext(Municipal FE, NO, Year FE, NO, Party FE, NO) addstat("No. municipalities", e(N_clust)) noni eform append 
logit nosingle onepartyrule i.year, or cluster(mc)
	*outreg2 using tablea12, excel label sortvar(onepartyrule stabdum2 stabdum3) drop(i.mc i.mayoralparty i.year ) dec(2) addtext(Municipal FE, NO, Year FE, YES, Party FE, NO) addstat("No. municipalities", e(N_clust)) noni eform append 
logit nosingle onepartyrule scb_medinc i.mayoralparty i.year pop6dum2-pop6dum5 ncmc_sb_yr ncmc_sb_yr2, or cluster(mc)
	*outreg2 using tablea12, excel label sortvar(onepartyrule stabdum2 stabdum3) drop(i.mc i.mayoralparty i.year ) dec(2) addtext(Municipal FE, NO, Year FE, YES, Party FE, YES) addstat("No. municipalities", e(N_clust)) noni eform append 
xtlogit nosingle stabdum2 stabdum3, or fe
	*outreg2 using tablea12, excel label sortvar(onepartyrule stabdum2 stabdum3) drop(i.mc i.mayoralparty i.year ) dec(2) addtext(Municipal FE, YES, Year FE, NO, Party FE, NO) cttop(FE logit) addstat("No. municipalities", e(N_g)) noni eform append 
xtlogit nosingle stabdum2 stabdum3 scb_medinc, or fe
	*outreg2 using tablea12, excel label sortvar(onepartyrule stabdum2 stabdum3) drop(i.mc i.mayoralparty i.year ) dec(2) addtext(Municipal FE, YES, Year FE, NO, Party FE, NO) addstat("No. municipalities", e(N_g)) noni eform append 
xtlogit nosingle stabdum2 stabdum3 i.mayoralparty, or fe
	*outreg2 using tablea12, excel label sortvar(onepartyrule stabdum2 stabdum3) drop(i.mc i.mayoralparty i.year ) dec(2) addtext(Municipal FE, YES, Year FE, NO, Party FE, YES) addstat("No. municipalities", e(N_g)) noni eform append 
xtlogit nosingle stabdum2 stabdum3 i.year, or fe
	*outreg2 using tablea12, excel label sortvar(onepartyrule stabdum2 stabdum3) drop(i.mc i.mayoralparty i.year ) dec(2) addtext(Municipal FE, YES, Year FE, YES, Party FE, NO) addstat("No. municipalities", e(N_g)) noni eform append 
xtlogit nosingle stabdum2 stabdum3 ncmc_sb_yr ncmc_sb_yr2, or fe
	*outreg2 using tablea12, excel label sortvar(onepartyrule stabdum2 stabdum3) drop(i.mc i.mayoralparty i.year ) dec(2) addtext(Municipal FE, YES, Year FE, NO, Party FE, NO) addstat("No. municipalities", e(N_g)) noni eform append 
xtlogit nosingle stabdum2 stabdum3 scb_medinc i.mayoralparty i.year ncmc_sb_yr ncmc_sb_yr2, or fe
	*outreg2 using tablea12, excel label sortvar(onepartyrule stabdum2 stabdum3) drop(i.mc i.mayoralparty i.year ) dec(2) addtext(Municipal FE, YES, Year FE, YES, Party FE, YES) addstat("No. municipalities", e(N_g)) noni eform append 
********************************************************************************************************************************************************************************************************************************************************************************************************************************
*** TABLE A13. NUMBER OF BIDS
reg nobids_mps onepartyrule if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea13, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, NO) cttop("Cross-section OLS") ///
			*addnote("Dependent variable: Average number of bids (discounted). Note that the discounting procedure (1/[number of bidders2]) means that higher number of bidders generate lower scores, and vice versa. Data averaged for the 2011-14 term period in models with One-party rule. Robust standard errors in parentheses; *** p<0.01 ** p<0.05 * p<0.1). GMM models use the twostep estimator with Windmeijer correction; Vote share, ruling party & LDV treated as predetermined and instrumented GMM-style (lag depth 1-4). Other covariates treated as exogenous and instrumented IV-style.") replace 
reg nobids_mps onepartyrule pop6dum2-pop6dum6 if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea13, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, NO) append 
reg nobids_mps onepartyrule logarea if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea13, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, NO) append 
reg nobids_mps onepartyrule scb_medinc_mps if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea13, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, NO) append 
reg nobids_mps onepartyrule i.mayoralparty if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea13, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, YES) append 
reg nobids_mps onepartyrule i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea13, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, YES, Party FE, NO) append 
reg nobids_mps onepartyrule pop6dum2-pop6dum6 logarea scb_medinc_mps i.mayoralparty i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea13, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, YES, Party FE, YES) append 
xtreg nobids stabdum2 stabdum3, fe robust
	*outreg2 using tablea13, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, No", "Party FEs:, No", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) cttop("Fixed Effects") append
xtreg nobids stabdum2 stabdum3 scb_medinc , fe robust
	*outreg2 using tablea13, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, No", "Party FEs:, No", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtreg nobids stabdum2 stabdum3 i.mayoralparty , fe robust
	*outreg2 using tablea13, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, No", "Party FEs:, Yes", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtreg nobids stabdum2 stabdum3 i.year, fe robust
	*outreg2 using tablea13, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, Yes", "Party FEs:, No", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append	
xtreg nobids stabdum2 stabdum3 scb_medinc i.mayoralparty i.year, fe robust
	*outreg2 using tablea13, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtreg nobids ldv stabdum2 stabdum3 scb_medinc i.mayoralparty i.year, fe robust
	*outreg2 using tablea13, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtabond2 nobids stabdum2 stabdum3 ldv logarea scb_medinc pop6dum2-pop6dum6 i.mayoralparty i.reg i.year, ///
 gmm(ldv stabdum2 stabdum3, lag(. 4)) iv(logarea scb_medinc pop6dum2-pop6dum6 i.mayoralparty i.reg i.year) two robust artest(5)
	*outreg2 using tablea13, excel label dec(2) ///
	*	addstat("No. municipalities", e(N_g), "AR1 (p)", e(ar1p), "AR2 (p)", e(ar2p), "Hansen J statistic (p)", e(hansenp), "No. Instruments", e(j)) cttop("Sys-GMM") ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, No", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, Yes") noni append							
xtabond2 nobids stabdum2 stabdum3 ldv scb_medinc i.mayoralparty i.year , ///
 gmm(ldv stabdum2 stabdum3, lag(. 4)) iv(scb_medinc i.mayoralparty i.year) two robust nolevel artest(5)
	*outreg2 using tablea13, excel label dec(2) ///
	*	addstat("No. municipalities", e(N_g), "AR1 (p)", e(ar1p), "AR2 (p)", e(ar2p), "Hansen J statistic (p)", e(hansenp), "No. Instruments", e(j)) cttop("Diff-GMM") ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, No", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, No") noni append				
*** TABLE A14. STABILITY (SMALL/LARGE MUNIS) ***************************************************************************************************************************************************
	*SMALL MUNIS 
xtreg sb stabdum2 stabdum3 if minpop<=15190.50, fe robust
	*outreg2 using tablea14, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, No", "Party FEs:, No", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) cttop(Small municipalities) cttop(Fixed effects) ///
			*addnote("Dependent variable: Single bidding ratio. Sample split on median (minimun) population size (15190.5). Robust standard errors in parentheses; *** p<0.01 ** p<0.05 * p<0.1). GMM models use the twostep estimator with Windmeijer correction; Vote share, ruling party & LDV treated as predetermined and instrumented GMM-style (lag depth 1-4). Other covariates treated as exogenous and instrumented IV-style") replace 
xtreg sb stabdum2 stabdum3 scb_medinc if minpop<=15190.50, fe robust
	*outreg2 using tablea14, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, No", "Party FEs:, No", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtreg sb stabdum2 stabdum3 i.mayoralparty if minpop<=15190.50, fe robust
	*outreg2 using tablea14, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, No", "Party FEs:, Yes", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtreg sb stabdum2 stabdum3 i.year if minpop<=15190.50, fe robust
	*outreg2 using tablea14, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, Yes", "Party FEs:, No", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append	
xtreg sb stabdum2 stabdum3 scb_medinc i.mayoralparty i.year if minpop<=15190.50, fe robust
	*outreg2 using tablea14, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtreg sb ldv stabdum2 stabdum3 scb_medinc i.mayoralparty i.year if minpop<=15190.50, fe robust
	*outreg2 using tablea14, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtabond2 sb stabdum2 stabdum3 ldv logarea scb_medinc pop6dum2-pop6dum6 i.mayoralparty i.reg i.year if minpop<=15190.50, ///
 gmm(ldv stabdum2 stabdum3, lag(. 4)) iv(logarea scb_medinc pop6dum2-pop6dum6 i.mayoralparty i.reg i.year) two robust artest(5)
*outreg2 using tablea14, excel label dec(2) ///
	*	addstat("No. municipalities", e(N_g), "AR1 (p)", e(ar1p), "AR2 (p)", e(ar2p), "Hansen J statistic (p)", e(hansenp), "No. Instruments", e(j)) cttop("Sys-GMM") ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, No", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, Yes") noni append				
xtabond2 sb stabdum2 stabdum3 ldv scb_medinc i.mayoralparty i.year if minpop<=15190.50, ///
 gmm(ldv stabdum2 stabdum3, lag(. 4)) iv(scb_medinc i.mayoralparty i.year) two robust nolevel artest(5)
*outreg2 using tablea14, excel label dec(2) ///
	*	addstat("No. municipalities", e(N_g), "AR1 (p)", e(ar1p), "AR2 (p)", e(ar2p), "Hansen J statistic (p)", e(hansenp), "No. Instruments", e(j)) cttop("Diff-GMM") ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, No", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, No") noni append				
	*LARGE MUNIS 
xtreg sb stabdum2 stabdum3 if minpop>15190.50, fe robust
	*outreg2 using tablea14, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, No", "Party FEs:, No", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) cttop(Large municipalities) cttop(Fixed effects) append
xtreg sb stabdum2 stabdum3 scb_medinc if minpop>15190.50, fe robust
	*outreg2 using tablea14, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, No", "Party FEs:, No", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtreg sb stabdum2 stabdum3 i.mayoralparty if minpop>15190.50, fe robust
	*outreg2 using tablea14, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, No", "Party FEs:, Yes", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtreg sb stabdum2 stabdum3 i.year if minpop>15190.50, fe robust
	*outreg2 using tablea14, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, Yes", "Party FEs:, No", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append	
xtreg sb stabdum2 stabdum3 scb_medinc i.mayoralparty i.year if minpop>15190.50, fe robust
	*outreg2 using tablea14, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtreg sb ldv stabdum2 stabdum3 scb_medinc i.mayoralparty i.year if minpop>15190.50, fe robust
	*outreg2 using tablea14, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtabond2 sb stabdum2 stabdum3 ldv logarea scb_medinc pop6dum2-pop6dum6 i.mayoralparty i.reg i.year if minpop>15190.50, ///
 gmm(ldv stabdum2 stabdum3, lag(. 4)) iv(logarea scb_medinc pop6dum2-pop6dum6 i.mayoralparty i.reg i.year) two robust artest(5)
*outreg2 using tablea14, excel label dec(2) ///
	*	addstat("No. municipalities", e(N_g), "AR1 (p)", e(ar1p), "AR2 (p)", e(ar2p), "Hansen J statistic (p)", e(hansenp), "No. Instruments", e(j)) cttop("Sys-GMM") ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, No", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, Yes") noni append				
xtabond2 sb stabdum2 stabdum3 ldv scb_medinc i.mayoralparty i.year if minpop>15190.50, ///
 gmm(ldv stabdum2 stabdum3, lag(. 4)) iv(scb_medinc i.mayoralparty i.year) two robust nolevel artest(5)
*outreg2 using tablea14, excel label dec(2) ///
	*	addstat("No. municipalities", e(N_g), "AR1 (p)", e(ar1p), "AR2 (p)", e(ar2p), "Hansen J statistic (p)", e(hansenp), "No. Instruments", e(j)) cttop("Diff-GMM") ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, No", "Year FEs:, Yes", "Party FEs:, Yes", "County FEs:, No") noni append				
*** TABLE A15. ONE-PARTY RULE X LOG POPULATION ***************************************************************************************************************************************************
reg sb_mps onepartyrule logpop_mps oprxlogpop_mps if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea15, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, NO) ///
			*addnote("Dependent variable: Single bidding. Robust standard errors in parentheses; *** p<0.01 ** p<0.05 * p<0.1") replace 
reg sb_mps onepartyrule logpop_mps oprxlogpop_mps logarea if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea15, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, NO) append 
reg sb_mps onepartyrule logpop_mps oprxlogpop_mps scb_medinc_mps if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea15, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, NO) append 
reg sb_mps onepartyrule logpop_mps oprxlogpop_mps i.mayoralparty if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea15, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, YES) append 
reg sb_mps onepartyrule logpop_mps oprxlogpop_mps i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea15, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, YES, Party FE, NO) append 
reg sb_mps onepartyrule logpop_mps oprxlogpop_mps logarea scb_medinc_mps i.mayoralparty i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea15, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO, Party FE, NO) append 

quietly reg sb_mps onepartyrule##c.logpop logarea scb_medinc_mps i.mayoralparty i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	margins, dydx(onepartyrule) at(logpop=(7.8240460109 8.5171931914 9.210340372 9.6158054801 10.308952661 11.512925465 12.429216197 13.710150042) reg=14 mayoralparty=1) atmeans 
	marginsplot, ///
	yline(0) ///
	xscale( noline) xlabel(, grid notick) xtitle("Population", size(medlarge)) ///
	plotregion(style(none)) graphregion(margin(l=-3 r-2)) ///
	yscale( noline axis(1)) ylabel(, grid notick axis(1)) ytitle("Predicted single bidding ratio", size(medlarge) height(6)) title("") ///
	xsize(3) ysize(2) legend(off) ///
	addplot(histogram logpop if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & contracts_sb_mps!=., ///
			barw(.2) freq yaxis(2) bc(none) ///
			ylabel(0(15)60, axis(2) labs(medium) nogrid notick) yscale(noline alt axis(2)) ///
			ytitle("Number of municipalities", size(medlarge) axis(2)) ///
			xlabel(7.8240460109 "2,500" 8.5171931914 "5,000" 9.210340372 "10,000" 9.6158054801 "15,000" 10.308952661 "30,000" 11.512925465 "100,000" 12.429216197 "250,000" 13.710150042 "900,000", angle(45) labc(black)))
	*graph export figurea5.png, replace
*** TABLE A16. ONE PARTY RULE, INTERACTED BY PARTY ID ***************************************************************************************************************************************************
reg sb_mps onepartyrule##i.mayoralparty_aud if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea16, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO) ///
			*addnote("Dependent variable: Single bidding. Other ruling party include Liberals (5), Christian Democrats (4), Left Party (2), and local parties (1). Robust standard errors in parentheses; *** p<0.01 ** p<0.05 * p<0.1") replace 
reg sb_mps onepartyrule##i.mayoralparty_aud pop6dum2-pop6dum6 if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea16, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO) append
reg sb_mps onepartyrule##i.mayoralparty_aud logarea if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea16, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO) append
reg sb_mps onepartyrule##i.mayoralparty_aud scb_medinc_mps if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea16, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, NO) append
reg sb_mps onepartyrule##i.mayoralparty_aud i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea16, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, YES) append
reg sb_mps onepartyrule##i.mayoralparty_aud pop6dum2-pop6dum6 logarea scb_medinc_mps i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust
	*outreg2 using tablea16, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty i.reg) addtext(County FE, YES) append

*** FIGURE A6.	
reg sb_mps onepartyrule##i.mayoralparty_aud i.pop6cat logarea scb_medinc_mps i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2, robust	
margins onepartyrule, at(mayoralparty_aud==(0 1 2 3) reg=14 pop6cat=4) atmeans
marginsplot, ///
	yscale(r(0 35) noline axis(1)) ylabel(0(5)25, grid notick axis(1)) ytitle("Predicted single bidding ratio", size(medlarge) height(6)) title("") ///
	xscale( noline) xlabel(, notick) xtitle("Ruling party", size(medlarge)) ///
	plotregion(style(none)) graphregion(margin(l=-3 r-2)) ///
	plot1opts(msymbol(none) msize(medlarge) mc(gs6) lc(gs6) lw(thick) lp(dash)) ci1opts(color(gs6)) ///
	plot2opts(msymbol(none) msize(medlarge) mc(black) lc(black) lw(thick) lp(solid)) ci2opts(color(black)) /// 
	xsize(3) ysize(2) ///
		addplot(histogram mayoralparty_aud if onepartyrule==1 & m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & contracts_sb_mps!=., ///
			discrete barw(.2) freq yaxis(2) bc(none) lc(black) lw(thick) ///
			ylabel(0(25)125, axis(2) labs(medium) nogrid notick) yscale(r(0 128) noline alt axis(2) ) ///
			ytitle("Number of municipal-years", size(medlarge) axis(2)) ///
			|| histogram mayoralparty_aud if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & contracts_sb_mps!=., discrete barw(.22) freq yaxis(2) bc(none) ///
			legend( region(lstyle(none) m(l=-11 r=-11 t=0 b=0)) symx(*.5) ///
				order(4 "P(Single bidding ratio | One party rule)" 3 "P(Single bidding ratio | Turnover)" 5 "N(One party rule municipalities)" 6 "N(All municipalities)" ))) ///
					name(figurea6, replace) 
			gr_edit .yaxis1.reset_rule 0 25 5 , tickset(major) ruletype(range) editcopy
	*graph export figurea6.png, replace
*** TABLE A17. STABILITY, INTERACTED BY PARTY ID ***************************************************************************************************************************************************
xtreg sb i.stability##i.mayoralparty_aud, fe robust
	*outreg2 using tablea17, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, No", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) cttop(Fixed effects) ///
			*addnote("Dependent variable: Single bidding ratio. Robust standard errors in parentheses; *** p<0.01 ** p<0.05 * p<0.1). GMM models use the twostep estimator with Windmeijer correction; Vote share, ruling party & LDV treated as predetermined and instrumented GMM-style (lag depth 1-4). Other covariates treated as exogenous and instrumented IV-style.") replace 
xtreg sb i.stability##i.mayoralparty_aud scb_medinc, fe robust
	*outreg2 using tablea17, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, No", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) cttop(Fixed effects) append
xtreg sb i.stability##i.mayoralparty_aud i.year, fe robust
	*outreg2 using tablea17, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, Yes", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) cttop(Fixed effects) append
xtreg sb i.stability##i.mayoralparty_aud scb_medinc i.year, fe robust
	*outreg2 using tablea17, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, Yes", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) cttop(Fixed effects) append
xtreg sb ldv i.stability##i.mayoralparty_aud scb_medinc i.year, fe robust
	*outreg2 using tablea17, excel label dec(2) ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, Yes", "Year FEs:, Yes", "County FEs:, No") noni addstat("No. municipalities", e(N_g)) append
xtabond2 sb i.stability##i.mayoralparty_aud ldv logarea scb_medinc pop6dum2-pop6dum6 i.mayoralparty i.reg i.year, ///
 gmm(ldv i.stability#i.mayoralparty_aud, lag(. 4)) iv(logarea i.mayoralparty_aud scb_medinc pop6dum2-pop6dum6 i.reg i.year) two robust artest(5)
	*outreg2 using tablea17, excel label dec(2) ///
	*	addstat("No. municipalities", e(N_g), "AR1 (p)", e(ar1p), "AR2 (p)", e(ar2p), "Hansen J statistic (p)", e(hansenp), "No. Instruments", e(j)) cttop("Sys-GMM") ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, No", "Year FEs:, Yes", "County FEs:, Yes") noni append				
xtabond2 sb i.stability##i.mayoralparty_aud ldv scb_medinc i.year, ///
 gmm(ldv i.stability#i.mayoralparty_aud, lag(. 4)) iv(scb_medinc i.mayoralparty_aud i.year) two robust nolevel artest(5)
	*outreg2 using tablea17, excel label dec(2) ///
	*	addstat("No. municipalities", e(N_g), "AR1 (p)", e(ar1p), "AR2 (p)", e(ar2p), "Hansen J statistic (p)", e(hansenp), "No. Instruments", e(j)) cttop("Diff-GMM") ///
	*	drop(i.year i.mayoralparty i.reg) addtext("Municipality FEs:, No", "Year FEs:, Yes", "County FEs:, NO") noni append				

*** FIGURE A7.
xtreg sb i.stability##i.mayoralparty_aud scb_medinc i.year, fe robust
margins mayoralparty_aud, at(stability==(0 1 2) year=2012) atmeans
marginsplot, ytitle("Predicted single bidding ratio", size(medlarge) height(6)) ///
	graphregion(margin(r=10)) plotregion(style(none)) graphregion(margin(l=-3 r-2)) title("") ylab(, grid) noci yscale(noline axis(1)) xscale( noline) xlabel(, notick) ylabel(, notick) ///
	legend( region(lstyle(none) m(l=-11 r=-11 t=0 b=0)) symx(*.5)) ///
	name(figurea7, replace)
	*graph export figurea7.png, replace

*** TABLE A18. MAJORITY/OPPOSITION RELATIONS ***************************************************************************************************************************************************
reg kfu_majoppfriends onepartyrule if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=., robust
	*outreg2 using tablea18, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.reg) addtext(County FE, NO, Party FE, NO) cttop("DV: Majority/opposition relations") ///
	*addnote("Data averaged for the 2011-14 term period. Robust standard errors in parentheses; *** p<0.01 ** p<0.05 * p<0.1") replace 
reg kfu_majoppfriends onepartyrule pop6dum2-pop6dum6 logarea scb_medinc_mps i.mayoralparty i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=. , robust
	*outreg2 using tablea18, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.reg) addtext(County FE, YES, Party FE, YES) append
reg sb_mps kfu_majoppfriends if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=., robust
	*outreg2 using tablea18, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.reg) addtext(County FE, NO, Party FE, NO) cttop("DV: Single bidding ratio") append
reg sb_mps kfu_majoppfriends pop6dum2-pop6dum6 logarea scb_medinc_mps i.mayoralparty i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=. , robust
	*outreg2 using tablea18, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.reg) addtext(County FE, NO, Party FE, NO) append
*** TABLE A19. AUDIT CONTROL ***************************************************************************************************************************************************
logit audmaj_full onepartyrule if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=., robust or
	*outreg2 using tablea19, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.nuts1) addtext(NUTS1 FE, NO, Party FE, NO) ///
	*addnote("Data averaged for the 2011-14 term period. Odds ratios displayed for models predicting Audit chair from majority (columns 1 & 2). Since a number of counties and mayoral parties perfectly predict Audit chair from majority, and are dropped from logit models, these have been substituted by NUTS1-region and the constrained mayoral party ID-variable used in table A13 & A14. Robust standard errors in parentheses; *** p<0.01 ** p<0.05 * p<0.1") cttop("DV: Audit chair from majority") eform replace 
logit audmaj_full onepartyrule pop6dum2-pop6dum6 logarea scb_medinc_mps i.mayoralparty_aud i.nuts1 if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=., robust or
	*outreg2 using tablea19, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.nuts1) addtext(NUTS1 FE, YES, Party FE, YES) eform append
reg sb_mps audmaj_full if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 , robust
	*outreg2 using tablea19, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.reg) addtext(County FE, NO, Party FE, NO) cttop("DV: Single bidding ratio") append
reg sb_mps audmaj_full pop6dum2-pop6dum6 logarea scb_medinc_mps i.mayoralparty_aud i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 , robust
	*outreg2 using tablea19, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.reg) addtext(County FE, YES, Party FE, YES) append
*** TABLE A20. MEDIA PRESSURE ***************************************************************************************************************************************************
reg mediapromise_maj onepartyrule if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=., robust
	*outreg2 using tablea20, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.reg) addtext(County FE, NO, Party FE, NO) cttop("DV: Media influence") ///
	*addnote("Data averaged for the 2011-14 term period. Robust standard errors in parentheses; *** p<0.01 ** p<0.05 * p<0.1") replace 
reg mediapromise_maj onepartyrule pop6dum2-pop6dum6 logarea scb_medinc_mps i.mayoralparty i.reg coverage if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=. , robust
	*outreg2 using tablea20, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.reg) addtext(County FE, YES, Party FE, YES) append
reg sb_mps mediapromise_maj if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=., robust
	*outreg2 using tablea20, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.reg) addtext(County FE, NO, Party FE, NO) cttop("DV: Single bidding ratio") append
reg sb_mps mediapromise_maj pop6dum2-pop6dum6 logarea scb_medinc_mps i.mayoralparty i.reg coverage if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=. , robust
	*outreg2 using tablea20, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.reg) addtext(County FE, NO, Party FE, NO) append
*** TABLE A21. BUREAUCRATIC HUMAN CAPITAL ***************************************************************************************************************************************************
reg kol_empedu_mps onepartyrule if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=., robust
	*outreg2 using tablea21, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.reg) addtext(County FE, NO, Party FE, NO) cttop("DV: bureaucratic human capital") ///
	*addnote("Data averaged for the 2011-14 term period. Robust standard errors in parentheses; *** p<0.01 ** p<0.05 * p<0.1") replace 
reg kol_empedu_mps onepartyrule pop6dum2-pop6dum6 logarea scb_medinc_mps i.mayoralparty i.reg scb_highered_mps kol_kop_egentlig_mps if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=. , robust
	*outreg2 using tablea21, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.reg) addtext(County FE, YES, Party FE, YES) append
reg sb_mps kol_empedu_mps if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=., robust
	*outreg2 using tablea21, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.reg) addtext(County FE, NO, Party FE, NO) cttop("DV: Single bidding ratio") append
reg sb_mps kol_empedu_mps pop6dum2-pop6dum6 logarea scb_medinc_mps i.mayoralparty i.reg scb_highered_mps kol_kop_egentlig_mps if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & sb_mps!=., robust
	*outreg2 using tablea21, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.reg) addtext(County FE, NO, Party FE, NO) append
*** TABLE A22. LOCAL WINNER ***************************************************************************************************************************************************
reg localwin_town_mps onepartyrule if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & contracts_lw_mis_mps>2, robust
	*outreg2 using tablea22, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.reg) addtext(County FE, NO, Party FE, NO) cttop("DV: Local winner") ///
	*addnote("Data averaged for the 2011-14 term period. Robust standard errors in parentheses; *** p<0.01 ** p<0.05 * p<0.1") replace 
reg localwin_town_mps onepartyrule pop6dum2-pop6dum6 logarea scb_medinc_mps i.mayoralparty i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & contracts_lw_mis_mps>2 , robust
	*outreg2 using tablea22, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.reg) addtext(County FE, YES, Party FE, YES) append
reg sb_mps localwin_town_mps if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & contracts_lw_mis_mps>2, robust
	*outreg2 using tablea22, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.reg) addtext(County FE, NO, Party FE, NO) cttop("DV: Single bidding ratio") append
reg sb_mps localwin_town_mps pop6dum2-pop6dum6 logarea scb_medinc_mps i.mayoralparty i.reg if m!="Dals-Ed" & year==2012 & contracts_sb_mps>2 & contracts_lw_mis_mps>2, robust
	*outreg2 using tablea22, excel label sortvar(onepartyrule) dec(2) drop(i.mayoralparty_aud i.reg) addtext(County FE, NO, Party FE, NO) append










