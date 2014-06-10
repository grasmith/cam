'resN4.prg  tables for Manifesto

'--- RESN4 social policies

pool p_eu eun de euw uk fr it es eus pl eue
'--- cost of social policy
smpl 1970 2030
p_eu.genr eub_gss_?_n4 = 0
smpl 2014 2030
p_eu.genr eub_gss_?_n4 = NLGADJ_?_n4
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
series sxmu_eu_ins_n4 = 0
'--- cost from N3
smpl 1970 2030
p_eu.genr eub_ipn_?_n3 = 0
smpl 2014 2030
p_eu.genr eub_ipn_?_n3 = IP_?_n3-exp(log(IP_?_n3)-IP_?_ins_n3)
smpl 1970 2030
series s = 0
for %b eun de euw uk fr it es eus pl eue
 s = s + eub_ipn_{%b}_n3*rx_{%b}_n3
next 
series eub_ipn$_n3 = s
p_eu.genr eub_ipv_?_n3 = 100*eub_ipn_?_n3/vv_?_n3
p_eu.genr eub_ipvc_?_n3 =  100*@cumsum(eub_ipn_?_n3)/vv_?_n3
series eub_ipv_eu_n3 = 100*s/vv$_eu_n3
series eub_ipvc_eu_n3 = 100*@cumsum(s)/vv$_eu_n3
series sxmu_eu_ins_n3 = 0

'--- outcomes
pool p_eu eu eun de euw uk fr it es eus pl eue
smpl 2000 2000 2008 2008 2010 2010 2013 2013 2020 2020 2030 2030
p_eu.makegroup GSS_?_n4 GSS_?_n3 GSS_?_n2 G_?_n4 G_?_n3 GV_?_n4 GV_?_n3 GV_?_n2 eub_gsv_?_n4 eub_gsvc_?_n4 VVN_?_n4 VVN_?_n3 VV_?_n4 VV_?_n3 VV_?_n2 VV_?_n1 NL_?_n4 NL_?_n3 eub_ipv_?_n4 eub_ipv_?_n3 eub_ipvc_?_n4 eub_ipvc_?_n3 XM$_?_n4 XM$_?_n3 sxmu_?_ins_n4 sxmu_?_ins_n3 IPV_?_n4 IPV_?_n3 NLGV_?_n4 NLGV_?_n2 LGV_?_n4 LGV_?_n3 LGOV_?_n4 LGOV_?_n2 100*LGF_?_n4/VV_?_n4 100*LGF_?_n2/VV_?_n2 CAV$_?_n4 CAV$_?_n2 GSS_?_n4*VVN_?_n4/100 GSS_?_n2*VVN_?_n2/100 NL_?_n1 NE_?_n4 VV_?_n4/NE_?_n4 NE_?_n1 VV_?_n1/NE_?_n1 NL_?_n2 NE_?_n2 VV_?_n2/NE_?_n2



