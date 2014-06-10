'resN2.prg  tables for Manifesto

'--- RESN2 apply SGP (60% debt ceiling)
'--- europe
pool p_eu eu eun de euw uk fr it es eus pl eue
smpl 2000 2000 2008 2008 2010 2010 2013 2013 2020 2020 2030 2030
'--- financial and macro outcomes
p_eu.makegroup LGV_?_n2 NLGV_?_n2 VV_?_n2/NL_?_n2 VV_?_n2 VV_?_n1 VVN_?_n2 VV_?_n1
