'PROGRAM: zdef.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6
'
' program segment to build inner and outer sub-models
'
' this segment loads estimated equations into sub-models and supplies
' the sub-model identities
'
' updated: FC 26/05/2012
'
' NB: unaligned behavioural variables assigned to the outer model are
' listed at the top of the routine below. Identities are assigned
' explicitly
'
'=====================================================================
subroutine pDefineModel

'--- clean up
delete *_tar
delete *_sav
delete m_*

call ListCol(t_Bloc, 1, nBloc, 1, " ", %blocs)

'--- declare two models
model m_wmi                    'inner model (aligned variables)
model m_wmo                    'outer model (unaligned variables)

'--- aligned behavioural variables
'    assign equations for these variables to the inner model
%tlva = ";YG;G;SP;IP;IV;is;im;rxu;pvi;"
for !j = 1 to nEq
  %var = t_Eq(!j,  2)
  if %var <> "sxm" then
    %a = "o"
    if @instr(%tlva, ";"+%var+";") > 0 then %a = "i" endif
    m_wm{%a}.append :p_{%var}
  endif
next
'--- world price of primary commodities (inner model)
m_wmi.append :eq_pa_w

'--- market shares for exports of manufactures (outer model)
'--- loop over partners
for !j = 1 to nBloc
  %p = t_Bloc(!j, 1)
  m_wmo.append :p_sxmu_{%p}

'--- for each partner, build a list of suppliers for which series
'    are available (when small blocs or individual countries are
'    used, some bilateral series may be missing).

'--- loop over suppliers to build the lists
  %l = ""
  for !i = 1 to nBloc
    %b = t_Bloc(!i, 1)
    %s = "sxmu_" + %b + "_"+ %p
    if @isobject(%s) then
      m_wmo.append @identity sxm_{%b}_{%p} = {%s}/sxmu_{%p}
      %l = %l + "+" + %s
    endif
  next

'--- sum predicted market shares for the partner
  series sxmu_{%p}=1
  m_wmo.append @identity sxmu_{%p} = {%l}
next

'============ 1: population, migration and employment
'             (outer model)

'--- adjust migration to sum to zero for world as a whole
'    the adjustment ratio to make the sum of negative values
'    equal to positive ones is r = 2*NIMU_W/(NIMUA_W-NIMU_W))
'    and the adjusted values are NIM = NIMU + r*(NIMU-abs(NIMU))/2
series NIMU_W = NIM_W
'--- sum of absolute values
call BlocList("abs(NIMU_?)", "+", %blocs, %s)
series NIMUA_W = {%s}

'--- note: employment calculated from transformed activity rates

call AppendIdent( _
  "W;o;" _
  + "N_? NIMU_? " _
  + "NIMUA_?=" + %s + " " _
+ ":B;o;" _
  + "NIM_?=NIMU_?+NIMU_W*(NIMU_?-@abs(NIMU_?))/(NIMUA_W-NIMU_W) " _
  + "NF_?=NF_?(-1)+DNNF_?+0.5*NIM_? " _
  + "N_?=N_?(-1)+DNN_?+NIM_? " _
  + "NM_?=N_?-NF_? " _
  + "NWPF_?=NF_?-NOPF_?-NCPF_? " _
  + "NWPM_?=NM_?-NOPM_?-NCPM_? " _
  + "NWP_?=NWPF_?+NWPM_? " _
  + "NEF_?=(NWPF_?(-1)+NOPF_?(-1))" _
    + "*((cafmax-cafmin)/(1/exp(NEAF_?)+1)+cafmin) " _
  + "NEM_?=(NWPM_?(-1)+NOPM_?(-1))" _
    + "*((cammax-cammin)/(1/exp(NEAM_?)+1)+cammin) " _
  + "NE_?=NEF_?+NEM_? " _
)

'==== 2: price base, domestic expenditure, c/a and trade balance

'--- income and transfer receipts and payments (outer model)
'    other variables (inner model)

'--- adjustment for real exchange rates
' the weighted average adjusted real exchange rate
'    sum(rxu*H)/(rxadj*H_W)
' should be constant, equal to the base year pp
' converter for world expenditure pp0_w
' implying the following identity
'    rxadj = sum(rxu*H)/(H_W*pp0w)

copy XIT$_* XIT$U_*
series XIT$U_W = XIT$_W
series rxadj = 1

call BlocList("rxu_?*H_?", "+", %blocs, %s)

call AppendIdent( _
  "W;i;" _
  + "H_? X$_? " _
  + "ph_?=ph_?(-1)*(1+pi_us/100)*rx_us(-1)/rx_us " _
  + "pp0_?=pp0_?(-1) " _
  + "rx_?(H_?) " _
  + "rxadj=(" + %s + ")/(H_?*pp0_?) " _
+ ":B;i;" _
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
+ ":W;o;" _
  + "XIT$U_? MIT$_? " _
+":B;o;" _
  + "XIT$U_?=NIT$U_?+@iif(BIT$U_?>0,BIT$U_?,0) " _
  + "XIT$_?=XIT$U_?*MIT$_W/XIT$U_W " _
  + "MIT$_?=XIT$U_?-BIT$U_? " _
  + "NIT$_?=@iif(XIT$_?<MIT$_?,XIT$_?,MIT$_?) " _
)

'============ 3: income and GDP
'             (inner model)

call AppendIdent( _
  "W;i;" _
  + "Y_? V_? " _
+ ":B;i;" _
  + "Y_?=H_?+CA$_?/rx_? " _
  + "Y$_?=Y_?*rx_? " _
  + "V_?=H_?+TB0_?/pp0_? " _
  + "V0_?=V_?*pp0_? " _
  + "VV$_?=H_?*rx_?+TB$_? " _
  + "VV_?=H_?+TB$_?/rx_? " _
  + "YN$_?=Y$_?/N_? " _
  + "VT_?=1.05*@movav(V_?,6)*exp(0.3*(log(V_?/V_?(-6)))) " _
+ ":BW;i;" _
  + "YN_?=Y_?/N_? " _
  + "YR_?=YN_?/YN_W " _
)

'======== 4: inflation, interest rates and nominal exchange rates
'         (inner model)

call AppendIdent( _
  "W;i;" _
  + "pi_?(H_?) " _
  + "pi$_?=100*(ph_?/ph_?(-1)-1) " _
  + "irs_?(H_?) irm_?(H_?) " _
+ ":B;i;" _
  + "pi_?=100*((1+pvi_?/100)*tt_?(-1)/tt_?-1) " _
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
'             (mixed model)

call AppendIdent( _
  "B;i;" _
  + "NLG_?=YG_?-G_? " _
+ ":B;o;" _
  + "slgx_?=1-log(1+YR_?)/2 " _
  + "rplgo_?=slgx_?*ph_?(-1)/ph_?+(1-slgx_?)*rpfa_? " _
  + "LG_?=AGF_?-NGF_?+LGADJ_? " _
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
  "B;i;" _
  + "C_?=YP_?-SP_? " _
  + "IPT_?=IP_?+IV_? " _
  + "YP_?=Y_?-YG_? " _
  + "NLP_?=SP_?-IPT_? " _
)

'============ 7: external position and banking system
'             (outer model)

copy AXO$_* AXO$U_*
copy NXF$_* NXF$U_*
call AppendIdent( _
  "W;o;" _
  + "AXO$U_? LX$_? R$_? " _
  + "AXO$_?=LX$_?-R$_? " _
+ ":B;o;" _
  + "AXO$U_?=NXI$_?+@iif(NXF$U_?>0,NXF$U_?,0) " _
  + "AXO$_?=AXO$U_?*AXO$_W/AXO$U_W " _
  + "LX$_?=AXO$U_?-NXF$U_? " _
  + "NX$_?=R$_?+NXF$_? " _
  + "NXF$_?=AXO$_?-LX$_? " _
  + "NXF$U_?=CA$_?-IR$_?+AXO$_?(-1)*rpaxo$u_?-LX$_?(-1)*rplx$_? " _
  + "NXN$_?=@iif(R$_?+AXO$_?<LX$_?,R$_?+AXO$_?,LX$_?) " _
  + "IR$_?=R$_?-R$_?(-1)*rpr$_? " _
  + "IAXO$_?=ILX$_?-IR$_?+CA$_? " _
  + "rpaxo$_?=(AXO$_?-IAXO$_?)/AXO$_?(-1) " _
  + "ILX$_?=LX$_?-LX$_?(-1)*rplx$_? " _
  + "DP_?=NFI_?+@iif(NFF_?>0,NFF_?,0) " _
  + "LN_?=DP_?-NFF_? " _
  + "NFF_?=R$_?/rx_?+LGF_?-AGF_? " _
)

'============ 8: write-offs, holding gains and domestic cash flows
'             (outer model)

call AppendIdent( _
  "B;o;" _
  + "HAGF_?=R$_?(-1)*rpr$_?/rx_?+(LN_?(-1)+LGF_?(-1)-DP_?(-1))*rpfa_?" _
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
'                 (mixed model)

call AppendIdent( _
  "B;i;" _
  + "KID_?=rdp*KI_?(-1) " _
  + "KI_?=KI_?(-1)-KID_?+IPT_? " _
  + "YPN_?=YP_?-KID_? " _
+ ":B;o;" _
  + "WP_?=KP_?+LGO_?+NFF_?+NXF$_?/rx_? " _
  + "HWP_?=HKP_?+HDP_?-HLN_?+HLGO_?+HAXO_?-HLX_? " _
  + "KP_?=pkp_?*KI_? " _
  + "rpkp_?=pkp_?/pkp_?(-1) " _
  + "HKP_?=KP_?-KP_?(-1)-IPT_?+KID_? " _
)

'============ 10: trade by commodity group
'             (outer model)

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
  "W;o;" _
  + "XA$U_? MA0U_? XE$U_? ME0U_? MM0U_? XS$U_? MS0U_? XMIN$_? " _
  + "XA0_? XE0_? XM0_? XS0_? MA$_? ME$_? MS$_? " _
+ ":B;o;" _
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
  m_wmo.append @identity XM$_{%b} = {%s}
  %s1 = "sxm_" + %b + "_"+ %b
  if @isobject(%s1) then
    %s = %s1 + "*MM$_" + %b
  else
    %s = "0"  
  endif
  m_wmo.append @identity XMIN$_{%b} = {%s}
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
  m_wmo.append @identity pmm0_{%b} = 1/({%s})
next

'============ 11: energy production and trade
'             (outer model)

p_Bloc.genr ttco2_?_ins = 0
call AppendIdent( _
  "W;o;" _
  + "EP_? ED_? " _ 
  + "pewsav=pe_w " _
  + "pe_?=pewsav*exp(100*log(ED_?/EP_?))" _
    + "/(1+(exp(100*log(ED_?/EP_?))-1)*400*pewsav/pepmax) " _
+ ":GW;o;" _
  + "CO2_? " _
+ ":B;o;" _
  + "pep_?=400*pe_w/rx_? " _
  + "ped_?=800*rx_?*EPN_?/ED_?" _
    + "+pep_?*(1-EPN_?/ED_?)+ttco2_?*CO2_?/ED_? " _
  + "pepc_?=pep_?+ttco2_?*CO2_?/(ED_?-EPN_?) " _
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
'             (outer model)
' (no identities)

'========= additional model variables (outer model)
%t = ""
for !i = 1 to nModelX
  %t = %t + t_ModelX(!i, 1) + ";o;" + t_ModelX(!i, 2) + ":"
next
call AppendIdent(%t)

endsub