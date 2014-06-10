'RES0.prg  baseline data for Manifesto

'--- RES0 baseline
'--- europe
pool p_eu eu eun de euw uk fr it es eus pl eue
smpl 2000 2000 2008 2008 2010 2010 2013 2013 2020 2020 2030 2030
'--- growth of Europe in the world
p_BGW.makegroup(gBGW) VVN_?_0 N_?_0 X$_?_0 VV_?_0 
show gBGW
'--- divergence in Europe
'    SGP and TSCG targets not met
p_eu.makegroup(gEU) VVN_?_0 LGV_?_0 NLGV_?_0 
show gEU

