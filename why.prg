'WHY.PRG  explain behaviour
'
' experimental program to analyze reasons for
' historical and projected behaviour of a variable
' in one bloc
'
' e.g. NULVM ES N4
'
'================================================
'--- model
'behaviour  d(x) = c + a0.x^1 + a1.y^1 + a2.dz + u
'where
'  v original value
'  x transformed value
'  y long-run determinants
'  dz short-run determinants
'  u residual

'--- analysis
'steady-state  ss: x* = -(c+a1.y)/a0
'gap           e = x-x* = (c+a0.x+a1.y)/a0
'actual change dx = a0.e^1 + a2.dz + u

'--- adjustment for historical mean of dynamic effects
'historical mean            ezm = mean(a2.dz)
'adjusted error correction  eca = ec+ezm
'adjusted dynamic effects   eza = ez-ezm
'actual change              dx = eca + eza + u

pageselect data
'NULVM ES N4

'--- p_NULVM representation
'-DLOG(25/(0.1+NULVM_ES)-1)-NULVM_ES_INS = C(16) + C(1) + C(2)*-LOG(25/(0.1+NULVM_ES(-1))-1) + C(3)*DLOG(NVM_ES(-1)) + C(4)*LOG(VVN_ES(-1))*DLOG(V_ES) + C(5)*LOG(VVN_ES(-1))*DLOG(V_ES(-1)) + C(6)*LOG(VVN_ES(-1))*DLOG(IP_ES) + C(7)*IV_W/V_W + C(8)*NUR_ES/N_ES + [AR(1)=C(9)]

'--- analysis in series prefixed by wh
smpl 1970 2030
series whv = NULVM_ES_n4
series whx = -LOG(25/(0.1+whv)-1)
'--- inverse transform
series whchk = 25/(exp(-whx)+1)-0.1-whv
'--- steady state components
series whss0 = -(p_nulvm.@coefs(16) + p_nulvm.@coefs(1))/p_nulvm.@coefs(2)
series whss1 = -p_nulvm.@coefs(3)*DLOG(NVM_ES_n4)/p_nulvm.@coefs(2)
series whss2 = -p_nulvm.@coefs(8)*NUR_ES_n4/N_ES_n4/p_nulvm.@coefs(2)
series whss = whss0+whss1+whss2
'--- adjusted steady state components
series whss1 = whss1-@mean(whss1, "1979 2010")
series whss2 = whss2-@mean(whss2, "1979 2010")
'--- steady state values of untransformed variable
series whssv = 25/(exp(-whss)+1)-0.1
'--- gap (actual less steady state)
series whe = whx - whss
'--- analysis of changes
series whdx = d(whx)
'--- error correction
series whec = p_nulvm.@coefs(2)*(whx(-1)-whss(-1))
'--- dynamic effects
series whz1 = p_nulvm.@coefs(7)*IV_W_n4/V_W_n4
series whz2 = p_nulvm.@coefs(4)*LOG(VVN_ES_n4(-1))*DLOG(V_ES_n4)
series whz3 = p_nulvm.@coefs(5)*LOG(VVN_ES_n4(-1))*DLOG(V_ES_n4(-1))
series whz4 = p_nulvm.@coefs(6)*LOG(VVN_ES_n4(-1))*DLOG(IP_ES_n4)
series whz = whz1+whz2+whz3+whz4
'--- residual
series whu = whdx-whec-whdz
'--- historical mean of dynamic effects
series whzm = @mean(whz, "1980 2011")
'--- adjusted error correction
series whec = whec+whzm
'--- adjusted dynamic effects
series whz = whz-whzm
series whz1 = whz1-@mean(whz1, "1980 2011")
series whz2 = whz2-@mean(whz2, "1980 2011")
series whz3 = whz3-@mean(whz3, "1980 2011")
series whz4 = whz4-@mean(whz4, "1980 2011")

smpl 1980 2030
group why whv whx whss whssv whss1 whss2 whe whdx whu whec whz whz1 whz2 whz3 whz4
show why
