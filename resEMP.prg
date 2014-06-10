'resEMP.prg  Presentation charts
'
' CAM version 5.1  EUR variant
'
' Charts for inclusion in PPT presentation of
' employment prospects in Europe
'
' Assumes SOLN4.wf1 which models an EU recovery
' programme
'
' This workfile includes results for N1 (targets),
' N2 (austerity) and N3 (industrial/location policy)
'
' creates gre_ charts on the graphs page
'
'=====================================
pageselect data

'--- EU plus member blocs
pool p_eu eu eun de euw uk fr it es eus pl eue
'--- member blocs only
pool p_eux eun de euw uk fr it es eus pl eue

'--- additional data for graphs
smpl 1970 2030
'--- world regions
p_group.genr dvv5_?_n2 = 100*(exp(log(vv_?_n2/vv_?_n2(-5))/5)-1)
p_group.genr lxvw_?_n2 = 100*lx$_?_n2/vv_w_n2
p_group.genr digw_?_n2 = 100*d(ipt_?_n2+g_?_n2)/vv_w_n2(-1)
p_group.genr NW_?_n2 = NVM_?_n2-0.7*NOM_?_n2+.86*NYM_?_n2+0.78*(NVF_?_n2-0.7*NOF_?_n2)+0.86*NYF_?_n2
p_group.genr NLW_?_n2 = 100*NL_?_n2/NW_?_n2 
p_group.genr NEW_?_n2 = 100*NE_?_n2/NW_?_n2 
'--- output per employee
series VVNE_w_n2 = VV_w_n2/NE_w_n2
p_group.genr VVNE_?_n2 = VV_?_n2/NE_?_n2
p_group.genr VVNEW_?_n2 = 100*VVNE_?_n2/@elem(VVNE_w_n2, "2013")

'--- europe baseline
p_eu.genr XN$_?_n2 = X$_?_n2/N_?_n2
p_eu.genr MV$_?_n2 = M$_?_n2/VV_?_n2
p_eu.genr XMV_?_n2 = XN$_?_n2/MV$_?_n2
p_eu.genr XMVEU_?_n2 = 100*XMV_?_n2/@elem(XMV_eu_n2, "2013")
p_eu.genr LGVEU_?_n2 = LG_?_n2/VV_eu_n2
'--- working population
p_eu.genr NW_?_n1 = NVM_?_n1-0.7*NOM_?_n1+.86*NYM_?_n1+0.78*(NVF_?_n1-0.7*NOF_?_n1)+0.86*NYF_?_n1
p_eu.genr NEW_?_n1 = 100*NE_?_n1/NW_?_n1 
p_eu.genr NW_?_n2 = NVM_?_n2-0.7*NOM_?_n2+.86*NYM_?_n2+0.78*(NVF_?_n2-0.7*NOF_?_n2)+0.86*NYF_?_n2
p_eu.genr NLW_?_n2 = 100*NL_?_n2/NW_?_n2 
p_eu.genr NEW_?_n2 = 100*NE_?_n2/NW_?_n2 
'--- resources for govt services
p_eu.genr GNGD_?_n2 = G_?_n2/(N_?_n2-0.5*NV_?_n2)
p_eu.genr GNGEU_?_n2 = 100*GNGD_?_n2/@elem(GNGD_eu_n2, "2013")
'--- recovery
p_eu.genr XN$_?_n4 = X$_?_n4/N_?_n4
p_eu.genr MV$_?_n4 = M$_?_n4/VV_?_n4
p_eu.genr XMV_?_n4 = XN$_?_n4/MV$_?_n4
p_eu.genr XMVEU_?_n4 = 100*XMV_?_n4/@elem(XMV_eu_n4, "2013")
p_eu.genr NW_?_n4 = NVM_?_n4-0.7*NOM_?_n4+.86*NYM_?_n4+0.78*(NVF_?_n4-0.7*NOF_?_n4)+0.86*NYF_?_n4
p_eu.genr NLW_?_n4 = 100*NL_?_n4/NW_?_n4 
p_eu.genr NEW_?_n4 = 100*NE_?_n4/NW_?_n4 
p_eu.genr GNGD_?_n4 = G_?_n4/(N_?_n4-0.5*NV_?_n4)
p_eu.genr GNGEU_?_n4 = 100*GNGD_?_n4/@elem(GNGD_eu_n4, "2013")
'--- output per employee
p_eu.genr VVNE_?_n2 = VV_?_n2/NE_?_n2
p_eu.genr VVNEU_?_n2 = 100*VVNE_?_n2/@elem(VVNE_eu_n2, "2013")
p_eu.genr VVNE_?_n4 = VV_?_n4/NE_?_n4
p_eu.genr VVNEU_?_n4 = 100*VVNE_?_n4/@elem(VVNE_eu_n4, "2013")

'--- cost of social policy
smpl 1970 2030
p_eu.genr eub_gss_?_n4 = 0
smpl 2014 2030
p_eux.genr eub_gss_?_n4 = NLGADJ_?_n4
smpl 1970 2030
series s = 0
for %b eun de euw uk fr it es eus pl eue
 s = s + eub_gss_{%b}_n4*rx_{%b}_n4
next 
series eub_gss$_n4 = s
p_eu.genr eub_gsv_?_n4 = 100*eub_gss_?_n4/vv_?_n4
p_eu.genr eub_gsvc_?_n4 =  100*@cumsum(eub_gss_?_n4)/vv_?_n4
series eub_gsv_eu_n4 = 100*s/vv$_eu_n4
series eub_gsvc_eu_n4 = 100*@cumsum(s)/vv$_eu_n4
'--- cost of investment subsidy
smpl 1970 2030
p_eu.genr eub_ipn_?_n4 = 0
smpl 2014 2030
p_eu.genr eub_ipn_?_n4 = IP_?_n4-exp(log(IP_?_n4)-IP_?_ins_n4)
smpl 1970 2030
series s = 0
for %b eun de euw uk fr it es eus pl eue
 s = s + eub_ipn_{%b}_n4*rx_{%b}_n4
next 
series eub_ipn$_n4 = s
p_eu.genr eub_ipv_?_n4 = 100*eub_ipn_?_n4/vv_?_n4
p_eu.genr eub_ipvc_?_n4 =  100*@cumsum(eub_ipn_?_n4)/vv_?_n4
series eub_ipv_eu_n4 = 100*s/vv$_eu_n4
series eub_ipvc_eu_n4 = 100*@cumsum(s)/vv$_eu_n4
'--- total cost
p_eu.genr eub_?_n4 = eub_ipv_?_n4+eub_gsv_?_n4
p_eu.genr eubc_?_n4 = eub_ipvc_?_n4+eub_gsvc_?_n4
p_eux.genr EUBV_?_n4 = 100*rx_{%b}_n4*(eub_gss_?_n4+eub_ipn_?_n4)/vv$_eu_n4

'--- build graphs
delete gre* graphs\gre*

'--- graph EU NES
smpl 1970 2030
graph gre.area(s) nea_eu_n2 nei_eu_n2 nes_eu_n2
copy gre greg
gre.addtext(t,font(20)) Employment in Europe by broad sector (millions)
gre.axis(l) range(0,240) font(20)
gre.axis(b) font(20)
gre.legend font(16)
gre.draw(shade,b,rgb(196,255,255)) 2014 2020
gre.draw(shade,b) 2020 2030
gre.setelem(1) legend(Agriculture) lwidth(1.3)
gre.setelem(2) legend(Industry) lwidth(1.3)
gre.setelem(3) legend(Services) lwidth(1.3)
gre.datelabel interval(obs,10,1900)
gre.options backcolor(255,255,196) background
copy gre gre_NESEU

'--- graph R DVV
delete gre greg
smpl 2000 2030
p_group.makegroup(greg) @pc(VV_?_n2)
freeze(gre) greg.line
call GRE_R("GDP growth rates")
copy gre gre_DVVR

'--- graph R LGV
delete gre greg
smpl 2000 2030
p_group.makegroup(greg) LGV_?_n2
freeze(gre) greg.line
call GRE_R("Government debt-to-GDP ratios")
copy gre gre_LGVR

'--- graph R LXVW
delete gre greg
smpl 1970 2030
p_group.makegroup(greg) LXVW_?_n2
freeze(gre) greg.area(s)
call GRE_R("External liabilities as % of world GDP")
copy gre gre_LXVWR

'--- graph R NEW
delete gre greg
smpl 1970 2030
p_group.makegroup(greg) NEW_?_n2
freeze(gre) greg.line
call GRE_R("Employment rates\nadjusted for demographic differences")
copy gre gre_NEWR

'--- graph R NEWS
delete gre greg
smpl 1980 2020
p_group.makegroup(greg) NEW_?_n2
freeze(gre) greg.line
call GRE_R("Employment rates\nadjusted for demographic differences")
copy gre gre_NEWSR

'--- graph R NNE
delete gre greg
smpl 1970 2030
p_group.makegroup(greg) NNE_?_n2/100
freeze(gre) greg.line
call GRE_R("Economic dependency ratios\ndependents per person employed")
copy gre gre_NNER

'--- graph E2 VVNEW
delete gre greg
smpl 2010 2030
p_group.makegroup(greg) VVNEW_?_n2
freeze(gre) greg.line
call GRE_R("Productivity (2013 world average = 100)")
gre.datelabel interval(obs,5,1900)
copy gre gre_VVNEW

'--- graph R DIGW
delete gre greg
smpl 1970 2030
p_group.makegroup(greg) DIGW_?_n2
freeze(gre) greg.area(s)
call GRE_R("Investment and govt services\nchanges as % of world GDP")
copy gre gre_DIGW

'--- graph E1 NEW
delete gre greg
smpl 1993 2030
p_eux.makegroup(greg) NEW_?_n1
freeze(gre) greg.line
%s = "'"
call GRE_E("Target employment rates\nadjusted for demographic differences")
copy gre gre_NEWE1

'--- graph E1 DV
delete gre greg
smpl 1993 2030
p_eux.makegroup(greg) @pc(VV_?_n1)
freeze(gre) greg.line
%s = "'"
call GRE_E("Target GDP growth (% p.a.)")
copy gre gre_DVV1

'--- graph E2 XMVEU
delete gre greg
smpl 2000 2030
p_eux.makegroup(greg) XMVEU_?_n2
freeze(gre) greg.line
call GRE_E("Competitiveness (Europe 2013 = 100)\nexports per capita divided by import propensity")
copy gre gre_XMVEU

'--- graph E2 LXV
delete gre greg
smpl 2000 2030
p_eux.makegroup(greg) LXV$_?_n2
freeze(gre) greg.line
call GRE_E("External liabilities as % of GDP")
copy gre gre_LXVE

'--- graph E2 LGV
delete gre greg
smpl 2000 2030
p_eux.makegroup(greg) LGV_?_n2
freeze(gre) greg.line
call GRE_E("Government debt as % of GDP")
copy gre gre_LGVE

'--- graph E2 LGVEU
delete gre greg
smpl 2000 2030
p_eux.makegroup(greg) LGVEU_?_n2
freeze(gre) greg.area(s)
call GRE_E("Government debt as % of Europe's GDP")
copy gre gre_LGVEU

'--- graph E2 VVNEU
delete gre greg
smpl 2010 2030
p_eux.makegroup(greg) VVNEU_?_n2
freeze(gre) greg.line
call GRE_E("Productivity (Europe 2013 = 100)")
gre.datelabel interval(obs,5,1900)
copy gre gre_VVNEU

'--- graph E2 NLW
delete gre greg
smpl 1993 2030
p_eux.makegroup(greg) NLW_?_n2
freeze(gre) greg.line
call GRE_E("Participation rates\nadjusted for demographic differences")
copy gre gre_NLWE

'--- graph E2 NEW
delete gre greg
smpl 1993 2030
p_eux.makegroup(greg) NEW_?_n2
freeze(gre) greg.line
call GRE_E("Employment rates\nadjusted for demographic differences")
copy gre gre_NEWE

'--- graph E2 NNE
delete gre greg
smpl 1990 2030
p_eux.makegroup(greg) NNE_?_n2/100
freeze(gre) greg.line
call GRE_E("Economic dependency ratios\ndependents per person employed")
copy gre gre_NNEE

'--- graph E4 XMVEU
delete gre greg
smpl 2000 2030
p_eux.makegroup(greg) XMVEU_?_n4
freeze(gre) greg.line
call GRE_E("Convergence: competitiveness (Europe 2013 = 100)\nexports per capita divided by import propensity")
copy gre gre_XMVEU4

'--- graph E4 GNGEU
delete gre greg
smpl 1993 2030
p_eux.makegroup(greg) GNGEU_?_n2
freeze(gre) greg.line
call GRE_E("Resources for govt services per dependent\nEurope 2013 = 100")
copy gre gre_GNGEU

'--- graph E4 VVNEU
delete gre greg
smpl 2010 2030
p_eux.makegroup(greg) VVNEU_?_n4
freeze(gre) greg.line
call GRE_E("Convergence: productivity (Europe 2013 = 100)")
gre.datelabel interval(obs,5,1900)
copy gre gre_VVNEU4

'--- graph E4 NEW
delete gre greg
smpl 1993 2030
p_eux.makegroup(greg) NEW_?_n4
freeze(gre) greg.line
call GRE_E("Convergence: employment rates\nadjusted for demographic differences")
copy gre gre_NEWE4

'--- graph E4 NUL
delete gre greg
smpl 1993 2030
p_eux.makegroup(greg) NUL_?_n4
freeze(gre) greg.line
call GRE_E("Convergence: unemployment rates")
copy gre gre_NULE4

'--- graph E4 IPVE
delete gre greg
smpl 1993 2030
p_eux.makegroup(greg) 100*IPT_?_n4/VV_?_n4
freeze(gre) greg.line
call GRE_E("Recovery: investment as % of GDP")
copy gre gre_IPVE4

'--- graph E4 GNGEU
delete gre greg
smpl 1993 2030
p_eux.makegroup(greg) GNGEU_?_n4
freeze(gre) greg.line
call GRE_E("Recovery: resources for govt services per dependent\nEurope 2013 = 100")
copy gre gre_GNGEU4

'--- graph E4 LGV
delete gre greg
smpl 2000 2030
p_eux.makegroup(greg) LGV_?_n4
freeze(gre) greg.line
call GRE_E("Recovery: government debt as % of GDP")
copy gre gre_LGVE4

'--- graph E4 EUBV
delete gre greg
smpl 2015 2030
p_eux.makegroup(greg) EUBV_?_n4
freeze(gre) greg.area(s)
call GRE_E("Recovery: programme cost as % of European GDP")
gre.axis(l) range(0,1.8)
gre.datelabel interval(obs,5,1900)
copy gre gre_EUBV4

'--- graph E4 YNE
delete gre greg
smpl 2014 2030
p_eux.makegroup(greg) YN_?_n4/YN_?_n2
freeze(gre) greg.line
call GRE_E("Recovery: impact on per capita income")
gre.datelabel interval(obs,5,1900)
copy gre gre_YNE4

delete gre greg
copy gre* graphs\gre*
delete gre*

subroutine GRE_R(string %p_t)
gre.addtext(t,font(20),just(c)) %p_t
gre.legend font(16)
gre.draw(shade,b,rgb(196,255,255)) 2014 2020
gre.draw(shade,b) 2020 2030
gre.setelem(1) legend(Europe) lwidth(1.3)
gre.setelem(2) legend(North America) lwidth(1.3)
gre.setelem(3) legend(Latin America) lwidth(1.3)
gre.setelem(4) legend(Africa) lwidth(1.3)
gre.setelem(5) legend(Other Asia) lwidth(1.3)
gre.setelem(6) legend(East Asia) lwidth(1.3)
gre.axis(l) zeroline font(20)
gre.axis(b) font(20)
gre.datelabel interval(obs,10,1900)
gre.options gridl gridwidth(0.5) gridcolor(gray) backcolor(255,255,196) background
endsub

subroutine GRE_E(string %p_t)
gre.addtext(t,font(20),just(c)) %p_t
gre.legend font(16)
gre.draw(shade,b,rgb(196,255,255)) 2014 2020
gre.draw(shade,b) 2020 2030
gre.setelem(1) legend(Nordic countries) lwidth(1.5)
gre.setelem(2) legend(Germany) lwidth(1.5)
gre.setelem(3) legend(Other West Europe) lwidth(1.5)
gre.setelem(4) legend(UK) lwidth(1.5)
gre.setelem(5) legend(France) lwidth(1.5)
gre.setelem(6) legend(Italy) lwidth(1.5)
gre.setelem(7) legend(Spain) lwidth(1.5)
gre.setelem(8) legend(Other South Europe) lwidth(1.5)
gre.setelem(9) legend(Poland) lwidth(1.5)
gre.setelem(10) legend(Other East Europe) lwidth(1.5)
gre.axis(l) zeroline font(20)
gre.axis(b) font(20)
gre.datelabel interval(obs,10,1900)
gre.options gridl gridwidth(0.5) gridcolor(gray) backcolor(255,255,196) background
endsub