'resN3.prg  tables for Manifesto

'--- RESN3 industrial and location policies

pool p_eu eun de euw uk fr it es eus pl eue
'--- cost of investment subsidy
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
p_eu.makegroup NL_?_n3 NL_?_n2 IPV_?_n3 IPV_?_n2 eub_ipv_?_n3 eub_ipvc_?_n3 XM$_?_n3 XM$_?_n2 sxmu_?_ins_n3 VVN_?_n3 VVN_?_n2 VV_?_n3 VV_?_n2 VV_?_n1 bmv$_?_n3 bmv$_?_n2 VV_?_n3/NE_?_n3 VV_?_n2/NE_?_n2 VV_?_n1/NE_?_n1
