'PROGRAM: zdef.prg      Copyright (C) 2012,2015 Alphametrics Co. Ltd.
'
' CAM Version 6.1
'
' program segment to build the core model
'
' updated: FC 1/05/2015
'
'=====================================================================
subroutine pDefineModel

'--- clean up
delete *_tar
delete *_sav
delete m_wm*

call ListCol(t_Bloc, 1, nBloc, 1, " ", %blocs)

'--- declare the core model
model m_wm

'--- equations for behavioural variables
for !j = 1 to nEq
  %var = t_Eq(!j,2)
  if %var <> "sxm" then m_wm.append :p_{%var} endif
next
'--- world price of primary commodities
m_wm.append :eq_pa_w

'--- market shares for exports of manufactures
'--- loop over blocs
for !j = 1 to nBloc
  %p = t_Bloc(!j, 1)
'--- include export share equations for each supplier
  m_wm.append :p_sxmu_{%p}
'--- for each partner, identify suppliers for which
'    series are available, add to a list of suppliers
'    and include a scaling adjustment identity so that
'    supplier market shares add up to 1
  %l = ""
  for !i = 1 to nBloc
    %b = t_Bloc(!i, 1)
    %s = "sxmu_" + %b + "_"+ %p
    if @isobject(%s) then
      m_wm.append @identity sxm_{%b}_{%p} = {%s}/sxmu_{%p}
      %l = %l + "+" + %s
    endif
  next
'--- sum predicted market shares for the partner
  series sxmu_{%p}=1
  m_wm.append @identity sxmu_{%p} = {%l}
next

'============ 1: population, migration and employment
'             (mixed model)

'--- adjust migration to sum to zero for world as a whole
'    the adjustment ratio to make the sum of negative values
'    equal to positive ones is r = 2*NIMU_W/(NIMUA_W-NIMU_W))
'    and the adjusted values are
'        NIM = NIMU + r*(NIMU-abs(NIMU))/2
series NIMU_W = NIM_W
'--- sum of absolute values
call BlocList("abs(NIMU_?)", "+", %blocs, %s)
series NIMUA_W = {%s}
call AppendIdent( _
  "W;" _
  + "JLA_? N_? NV_? NP_? NOP_? NWP_? NIMU_? " _
  + "NIMUA_?=" + %s + " " _
+ ":G;" _
  + "JLA_? N_? NV_? NP_? NOP_? NWP_? " _
+ ":B;" _
  + "JLA_?=JLA_?(-1) " _
  + "NIM_?=NIMU_?+NIMU_W*(NIMU_?-@abs(NIMU_?))" _
    + "/(NIMUA_W-NIMU_W) " _
  + "NVF_?=NVF_?(-1)+DNNVF_?+NIM_?/4 " _
  + "NVM_?=NVM_?(-1)+DNNVM_?+NIM_?/4 " _
  + "NV_?=NVM_?+NVF_? " _
  + "NYF_?=NYF_?(-1)+DNNYF_?+NIM_?/4 " _
  + "NYM_?=NYM_?(-1)+DNNYM_?+NIM_?/4 " _
  + "NY_?=NYM_?+NYF_? " _
  + "NOP_?=NOF_?+NOM_? " _
  + "NPF_?=NYF_?+NVF_? " _
  + "NPM_?=NYM_?+NVM_? " _
  + "NP_?=NPF_?+NPM_? " _
  + "N_?=NCP_?+NP_? " _
  + "NWP_?=N_?-NCP_?-NOP_? " _
  + "NLYF_?=NLNYF_?*NYF_?/100 " _
  + "NLYM_?=NLNYM_?*NYM_?/100 " _
  + "NLY_?=NLYF_?+NLYM_? " _
  + "NLVF_?=NLNVF_?*NVF_?/100 " _
  + "NLVM_?=NLNVM_?*NVM_?/100 " _
  + "NLV_?=NLVF_?+NLVM_? " _
  + "NLF_?=NLYF_?+NLVF_? " _
  + "NLM_?=NLYM_?+NLVM_? " _
  + "NUYF_?=NULYF_?*NLYF_?/100 " _
  + "NUYM_?=NULYM_?*NLYM_?/100 " _
  + "NUY_?=NUYF_?+NUYM_? " _
  + "NUVF_?=NULVF_?*NLVF_?/100 " _
  + "NUVM_?=NULVM_?*NLVM_?/100 " _
  + "NUV_?=NUVF_?+NUVM_? " _
  + "NUF_?=NUYF_?+NUVF_? " _
  + "NUM_?=NUYM_?+NUVM_? " _
  + "NEVF_?=NLVF_?-NUVF_? " _
  + "NEVM_?=NLVM_?-NUVM_? " _
  + "NEV_?=NEVF_?+NEVM_? " _
  + "NEYF_?=NLYF_?-NUYF_? " _
  + "NEYM_?=NLYM_?-NUYM_? " _
  + "NEY_?=NEYF_?+NEYM_? " _
  + "NEF_?=NEYF_?+NEVF_? " _
  + "NEM_?=NEYM_?+NEVM_? " _
  + "NEAF_?=NEAEF_?*NEF_?/100 " _
  + "NEAM_?=NEAEM_?*NEM_?/100 " _
  + "NENF_?=NEF_?-NEAF_? " _
  + "NENM_?=NEM_?-NEAM_? " _
  + "NEIF_?=NEINF_?*NENF_?/100 " _
  + "NEIM_?=NEINM_?*NENM_?/100 " _
  + "NESF_?=NENF_?-NEIF_? " _
  + "NESM_?=NENM_?-NEIM_? " _
  + "NL_?=NLF_?+NLM_? " _
  + "NU_?=NUF_?+NUM_? " _
  + "NE_?=NEF_?+NEM_? " _
  + "NEA_?=NEAF_?+NEAM_? " _
  + "NEN_?=NENF_?+NENM_? " _
  + "NEI_?=NEIF_?+NEIM_? " _
  + "NES_?=NESF_?+NESM_? " _
)

'==== 2: price base, domestic expenditure, c/a and trade balance

'--- income and transfer receipts and payments
'    other variables

'--- adjustment for real exchange rates
' the weighted average adjusted real exchange rate
'    sum(rxu*H)/(rxadj*H_W)
' should be constant, equal to the base year pp
' converter for world expenditure pp0w
' implying the following identity
'    rxadj = sum(rxu*H)/(H_W*pp0w)

copy XIT$_* XIT$U_*
series XIT$U_W = XIT$_W
series rxadj = 1

call BlocList("rxu_?*H_?", "+", %blocs, %s)

call AppendIdent( _
  "W;" _
  + "H_? X$_? " _
  + "ph_?=ph_?(-1)*(1+pi_us/100)*rx_us(-1)/rx_us " _
  + "rxadj=(" + %s + ")/(H_?*pp0w) " _
+ ":B;" _
  + "H_?=C_?+IP_?+IV_?+G_? " _
  + "pp0_?=pp0_?(-1) " _
  + "rx_?=rxu_?/rxadj " _
  + "ph_?=rx_?*ph_w " _
  + "X$_?=XA$_?+XE$_?+XM$_?+XS$_? " _
  + "M$_?=MA$_?+ME$_?+MM$_?+MS$_? " _
  + "X0_?=XA0_?+XE0_?+XM0_?+XS0_? " _
  + "M0_?=MA0_?+ME0_?+MM0_?+MS0_? " _
  + "TB$_?=X$_?-M$_? " _
  + "TB0_?=X0_?-M0_? " _
  + "TB_?=TB$_?/rx_? " _
  + "CA$_?=TB$_?+BIT$_? " _
  + "CA_?=CA$_?/rx_? " _
  + "tt_?=(H_?+TB_?)/(H_?+TB0_?/pp0_?) " _
  + "BIT$_?=XIT$_?-MIT$_? " _
+ ":W;" _
  + "XIT$U_? MIT$_? " _
+":B;" _
  + "XIT$U_?=NIT$U_?+@iif(BIT$U_?>0,BIT$U_?,0) " _
  + "XIT$_?=XIT$U_?*MIT$_W/XIT$U_W " _
  + "MIT$_?=XIT$U_?-BIT$U_? " _
  + "NIT$_?=@iif(XIT$_?<MIT$_?,XIT$_?,MIT$_?) " _
)

'============ 3: income and GDP
'            

call AppendIdent( _
  "W;" _
  + "Y_? V_? VV$_? VV_? VVN_?=VV_?/N_? " _
+ ":G;" _
  + "VV_? VVN_?=VV_?/N_? " _
+ ":B;" _
  + "Y_?=H_?+CA$_?/rx_? " _
  + "Y$_?=Y_?*rx_? " _
  + "V_?=H_?+TB0_?/pp0_? " _
  + "V0_?=V_?*pp0_? " _
  + "VV$_?=H_?*rx_?+TB$_? " _
  + "VV_?=H_?+TB$_?/rx_? " _
  + "VVN_?=VV_?/N_? " _
  + "YN$_?=Y$_?/N_? " _
  + "VVS_?=VV_?-VVA_?-VVE_?-VVI_? " _
  + "VVTX_?=rtx_?*VV_?/(100+rtx_?) " _
  + "VVPR_?=mu_?*(VV_?-VVTX_?)/(100+mu_?) " _
  + "VVEM_?=VV_?-VVPR_?-VVTX_? " _
  + "VVEMV_?=100*VVEM_?/VV_? " _
  + "VVEME_?=VVEM_?/NE_? " _
  + "VT_?=1.05*@movav(V_?,6)*exp(0.3*(log(V_?/V_?(-6)))) " _
+ ":BW;" _
  + "YN_?=Y_?/N_? " _
  + "YR_?=YN_?/YN_W " _
)

'======== 4: inflation, interest rates and nominal exchange rates
'        
'
' this version assumes behaviour is modelled by cost
' inflation pvi leaving prices and earnings to be derived
' by identity
'
' if behaviour is modelled by earnings inflation
' bloc identities for pi and ei below must be replaced by
' the following
'  + "pi_?=100*((1+ei_?/100)*VVEME_?(-1)/VVEME_?-1) " _
'  + "pvi_?=100*((1+pi_?/100)*tt_?/tt_?(-1)-1) " _

call AppendIdent( _
  "W;" _
  + "pi_?(H_?) " _
  + "pi$_?=100*(ph_?/ph_?(-1)-1) " _
  + "irs_?(H_?) irm_?(H_?) " _
+ ":B;" _
  + "pi_?=100*((1+pvi_?/100)*tt_?(-1)/tt_?-1) " _
  + "ei_?=100*((1+pi_?/100)*VVEME_?/VVEME_?(-1)-1) " _
  + "spvi_?=log(-0.718+3.436*(1+pvi_?/100)/(2+pvi_?/100)) " _
  + "rpfa_?=1/(1+spvi_?) " _
  + "pi$_?=100*(ph_?/ph_?(-1)-1) " _
  + "pvd_?=pvd_?(-1)*(1+pvi_?/100) " _
  + "phd_?=phd_?(-1)*(1+pi_?/100) " _
  + "irs_?=100*((1+is_?/100)/(1+pi_?/100)-1) " _
  + "irm_?=100*((1+im_?/100)/(1+pi_?/100)-1) " _
  + "rxna_?=100*((ph_?/ph_?(-1))/(1+pi_?/100)-1) " _
  + "rxd_?=rxd_?(-1)*(1+rxna_?/100) " _
  + "vx_?=2+(X0_?-M0_?)/(pp0_?*V_?) " _
  + "mh_?=M0_?/(pp0_?*H_?+vx_?*X0_?) " _
  + "ucx$_?=mh_?*vx_?*M$_?/M0_?+(1-mh_?*vx_?)*rx_?/pp0_? " _
)

'============ 5: government expenditure, income and debt

call AppendIdent( _
  "B;" _
  + "YG_?=YGR_?-YGTI_? " _
  + "NLG_?=YG_?-G_? " _
+ ":B;" _
  + "slgx_?=1-log(1+YR_?)/2 " _
  + "rplgo_?=slgx_?*ph_?(-1)/ph_?+(1-slgx_?)*rpfa_? " _
  + "LG_?=AGF_?-NGF_? " _
  + "LGF_?=LG_?-LGO_? " _
  + "AGF_?=NGI_?+@iif(NGF_?>0,NGF_?,0) " _
  + "NGF_?=NLG_?+AGF_?(-1)+HAGF_?-IAGO_?-LGO_?(-1)*rplgo_?" _
    + "-LGF_?(-1)*rpfa_? " _
  + "ILG_?=IAG_?-NLG_? " _
  + "ILGO_?=LGO_?-LGO_?(-1)*rplgo_? " _
  + "ILGF_?=ILG_?-ILGO_? " _
)

'============ 6: private expenditure and income

call AppendIdent( _
  "W;" _
  + "IP_? IV_? " _
+ ":B;" _
  + "C_?=YP_?-SP_? " _
  + "IPT_?=IP_?+IV_? " _
  + "YP_?=Y_?-YG_? " _
  + "NLP_?=SP_?-IPT_? " _
)

'============ 7: external position and banking system
'            

copy LOI$_* LOI$U_*
copy NOF$_* NOF$U_*
call AppendIdent( _
  "W;" _
  + "ILDI$U_? IADI$_? LDI$U_? ADI$_? " _
  + "LOI$U_? API$_? LPI$_? AOI$_? R$_? " _
  + "LOI$_?=API$_?+AOI$_?+R$_?-LPI$_? " _
+ ":B;" _
  + "ILDI$_?=ILDI$U_?*IADI$_W/ILDI$U_W " _
  + "LDI$_?=LDI$U_?*ADI$_W/LDI$U_W " _
  + "IOI$_?=CA$_?+ILDI$_?+ILPI$_?-IADI$_?-IAPI$_?-IR$_? " _
  + "NOF$U_?=AOI$_?(-1)-LOI$_?(-1)" _
    + "+HAOI$_?-HLOI$U_?+IOI$_? " _
  + "NOF$_?=AOI$_?-LOI$_? " _
  + "LOI$U_?=NOI$_?-@iif(NOF$U_?>=0,0,NOF$U_?) " _
  + "AOI$_?=LOI$U_?+NOF$U_? " _
  + "LOI$_?=LOI$U_?*LOI$_W/LOI$U_W " _
  + "IAOI$_?=AOI$_?-AOI$_?(-1)-HAOI$_? " _
  + "ILOI$_?=IAOI$_?-IOI$_? " _
  + "HLOI$_?=LOI$_?-LOI$_?(-1)-ILOI$_? " _
  + "AXO$_?=ADI$_?+API$_?+AOI$_? " _
  + "LX$_?=LDI$_?+LPI$_?+LOI$_? " _
  + "IAXO$_?=IADI$_?+IAPI$_?+IAOI$_? " _
  + "ILX$_?=ILDI$_?+ILPI$_?+ILOI$_? " _
  + "IR$_?=R$_?-R$_?(-1)-HR$_? " _
  + "HAXO$_?=AXO$_?-AXO$_?(-1)-IAXO$_? " _
  + "HLX$_?=LX$_?-LX$_?(-1)-ILX$_? " _
  + "NXF$_?=AXO$_?-LX$_? " _
  + "NX$_?=R$_?+NXF$_? " _
  + "NXN$_?=@iif(R$_?+AXO$_?<LX$_?,R$_?+AXO$_?,LX$_?) " _
  + "DP_?=NFI_?+@iif(NFF_?>0,NFF_?,0) " _
  + "LN_?=DP_?-NFF_? " _
  + "NFF_?=R$_?/rx_?+LGF_?-AGF_? " _
)

'============ 8: write-offs, holding gains and domestic cash flows
'            

'--- the private sector takes inflation-related capital
'    losses (rpfa) on deposits less advances (DP-LN)
'    the government takes the gain/loss on reserves
'    less bail-outs in case of major defaults (HAGF)
'--- government net investment in the banking system
'    (IAGF) is determined by the government's target
'    share of the balance sheet (AGF) and capital gains
'    or losses (HAGF)

call AppendIdent( _
  "B;" _
  + "HAGF_?=(R$_?(-1)+HR$_?)/rx_?+(LN_?(-1)+LGF_?(-1)-DP_?(-1))*rpfa_?" _
    + "-AGF_?(-1)-lnbail_?*LN_?(-1)*wln_?*rpfa_? " _
  + "IAGF_?=AGF_?-AGF_?(-1)-HAGF_? " _
  + "IAG_?=IAGF_?+IAGO_? " _
  + "WLNA_?=0.8*LN_?(-1)*wln_?*rpfa_?+0.2*WLNA_?(-1) " _
  + "ILN_?=LN_?-LN_?(-1)*rpfa_?*(1-wln_?) " _
  + "IDP_?=IR$_?/rx_?+ILN_?+ILGF_?-IAGF_? " _
  + "HDP_?=DP_?-DP_?(-1)-IDP_? " _
  + "HLN_?=LN_?-LN_?(-1)-ILN_? " _
  + "HLGO_?=LGO_?-LGO_?(-1)-ILGO_? " _
  + "HAXO_?=AXO$_?/rx_?-AXO$_?(-1)/rx_?(-1)-IAXO$_?/rx_? " _
  + "HLX_?=LX$_?/rx_?-LX$_?(-1)/rx_?(-1)-ILX$_?/rx_? " _
)

'================ 9: capital stock and wealth

call AppendIdent( _
  "B;" _
  + "KID_?=rdp*KI_?(-1) " _
  + "KI_?=KI_?(-1)-KID_?+IPT_? " _
  + "YPN_?=YP_?-KID_? " _
+ ":B;" _
  + "WP_?=KP_?+LGO_?+NFF_?+NXF$_?/rx_? " _
  + "HWP_?=HKP_?+HDP_?-HLN_?+HLGO_?+HAXO_?-HLX_? " _
  + "KP_?=pkp_?*KI_? " _
  + "rpkp_?=pkp_?/pkp_?(-1) " _
  + "HKP_?=KP_?-KP_?(-1)-IPT_?+KID_? " _
)

'============ 10: trade by commodity group
'            

copy MA0_* MA0U_*
copy XS$_* XS$U_*
series XA$U_W = XA$_W
series MA0U_W = MA0_W
series XE$U_W = XE$_W
series ME0U_W = ME0_W
series MM0U_W = MM0_W
series XS$U_W = XS$_W
series MS0U_W = MS0_W

call AppendIdent( _
  "W;" _
  + "XA$U_? MA0U_? XE$U_? ME0U_? MM0U_? XS$U_? MS0U_? " _
  + "XA0_? XE0_? XM0_? XS0_? MA$_? ME$_? MS$_? " _
+ ":B;" _
  + "lpa_?=0.3*log(pa_w/rx_?)+0.7*lpa_?(-1) " _
  + "MA0U_?=XA0_?-BA0U_? " _
  + "XS$U_?=BS$U_?+MS$_? " _
  + "XA$_?=XA$U_?*MA$_W/XA$U_W " _
  + "XE$_?=XE$U_?*ME$_W/XE$U_W " _
  + "XS$_?=XS$U_?*MS$_W/XS$U_W " _
  + "MA0_?=MA0U_?*XA0_W/MA0U_W " _
  + "ME0_?=ME0U_?*XE0_W/ME0U_W " _
  + "MM0_?=MM0U_?*XM0_W/MM0U_W " _
  + "MS0_?=MS0U_?*XS0_W/MS0U_W " _
  + "MMH_?=C_?+0.4*G_?+2*(IP_?+IV_?)+(X$_?+2*XM$_?)/rx_? " _
)

'--- total exports of manufactures from each bloc
for !i = 1 to nBloc
  %b = t_Bloc(!i, 1)
  %s = ""
  for !j = 1 to nBloc
    %p = t_Bloc(!j, 1)
    %s1 = "sxm_" + %b + "_"+ %p
    if @isobject(%s1) then
      %s = %s + "+" + %s1 + "*MM$_" + %p
    endif
  next
  m_wm.append @identity XM$_{%b} = {%s}
next

'--- weighted average supply price
for !i = 1 to nBloc
  %b = t_Bloc(!i, 1)
  %s = ""
  for !j = 1 to nBloc
    %p = t_Bloc(!j, 1)
    %s1 = "sxm_" + %p + "_" + %b
    if @isobject(%s1) then
      %s = %s + "+" + %s1 + "*XM0_" + %p + "/XM$_" + %p 
    endif
  next
  m_wm.append @identity pmm0_{%b} = 1/({%s})
next

'============ 11: energy production and trade
'            

p_Bloc.genr ttco2_?_ins = 0
scalar pewadj = 10
series pewsav
call AppendIdent( _
  "W;" _
  + "EP_? ED_? " _ 
  + "pewsav=pe_w " _
  + "pe_?=pewsav*exp(pewadj*log(ED_?/EP_?))" _
    + "/(1+(exp(pewadj*log(ED_?/EP_?))-1)*400*pewsav/pepmax) " _
+ ":GW;" _
  + "CO2_? " _
+ ":B;" _
  + "EPNX_?=@nan(EPNX_?(-1)*EX_?/EX_?(-1),0) " _
  + "EPNN_?=EPN_?-EPNX_? " _
  + "pep_?=400*pe_w/rx_? " _
  + "ped_?=800*rx_?*EPNN_?/ED_?" _
    + "+pep_?*(1-EPNN_?/ED_?)+ttco2_?*CO2_?/ED_? " _
  + "pepc_?=pep_?+ttco2_?*CO2_?/(ED_?-EPNN_?) " _
  + "lpep_?=0.15*log(pep_?*rx_?/(pepmax-400*pe_w))" _
    + "+0.85*lpep_?(-1) " _
  + "lped_?=0.3*log(ped_?*rx_?/(pepmax-400*pe_w))" _
    + "+0.7*lped_?(-1) " _
  + "lpepc_?=0.05*log(pepc_?*rx_?/(pepmax-400*pe_w))" _
    + "+0.95*lpepc_?(-1) " _
  + "ttco2_?=ttco2_?_ins " _
  + "lttco2_?=0.05*log(1+ttco2_?/pepmax)" _
    + "+0.95*lttco2_?(-1) " _
  + "EP_?=EPC_?+EPN_? " _
  + "EX_?=EP_?+EM_?-ED_? " _
)

'============ 12: well-being indicators
'            
' (no identities)

'========= additional model variables
%t = ""

for !i = 1 to nModelX
  %t = %t + t_ModelX(!i, 1) + ";" + t_ModelX(!i, 2) + ":"
next
call AppendIdent(%t)

endsub