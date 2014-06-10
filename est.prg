'PROGRAM: est.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6
'
' estimation of behavioural equations
'
' run this program after dat.prg (data preparation) and before
' sola.prg (alignment)
'
' the program reads DAT.wf1 and writes EST.wf1
'
' you can get different types of estimation output by switching
' on the various options listed below
'
' updated: FC 25/06/2012
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call est
'------------------------------------------------------------------
subroutine est

%equation_listing = "Yes"
%comparisons = "Yes"
%prediction_graphs = "Yes"
%startest = "1980"            ' first year for eqn estimation
%endest = "2010"              ' last year for eqn estimation

'--- "recent" intercept fitting for equations with option R
%startfit = "1996"            ' first year for "recent" intercept fit

'==================================================================
' PREFACE
'==================================================================
mode quiet

'--- open the history workfile
open DAT
pageselect graphs
delete *
pageselect tables
delete *
pageselect data
delete gp_gr* sp_log*

'--- update settings
call pLog("EST PROGRAM v0625")
%wkfile = "EST"
t_Settings(7,2) = %wkfile
wfsave {%wkfile}

'==================================================================
' EQUATION DEFINITIONS
'==================================================================

smpl %start %latest

call pLog("loading definitions of behavioural equations")

'--- bloc list
call ListCol(t_Bloc, 1, nBloc, 1, " ", %blocs)

'--- table storing equation definitions
table t_Eq
scalar nEq = 0

'========= EMPLOYMENT, MIGRATION and URBANISATION

'--- employment is bounded above (85%) and below (40%) relative to 
'    population of working age plus 20% of old people;
'    the elasticity of employment at the mid-point of the range is
'    six times the elasticity of the bounded ratio;
'    GDP growth terms are multiplied by relative income
'    making employment sensitive to GDP at the top end of the scale
'    and insensitive at the bottom end;
'    urbanisation is expected to have a negative effect on participation

'--- female and male activity rates using weighted population
'    with a transform to set bounds
scalar cafmax = 0.70
scalar cafmin = 0.15
p_Bloc.genr NEAF_? = log(1/((cafmax-cafmin) _
  /(NEF_?/(NWPF_?(-1)+NOPF_?(-1))-cafmin)-1))
scalar cammax = 0.95
scalar cammin = 0.45
p_Bloc.genr NEAM_? = log(1/((cammax-cammin) _
  /(NEM_?/(NWPM_?(-1)+NOPM_?(-1))-cammin)-1))

call AddEquation(%blocs, "NEAF female activity;;" _
  + "d(NEAF_?) = " _
  + "error correction:c*NEAF_?(-1);" _
  + "children:c*log(NCP_?(-1)/N_?(-1));" _
  + "GDP growth:c*YR_?(-1)*dlog(V_?);" _
  + "lagged GDP growth:c*YR_?(-1)*dlog(V_?(-1))")

call AddEquation(%blocs, "NEAM male activity;;" _
  + "d(NEAM_?) = " _
  + "error correction:c*NEAM_?(-1);" _
  + "urbanisation:c*log(NUR_?(-1)/N_?(-1));" _
  + "GDP growth:c*YR_?(-1)*dlog(V_?);" _
  + "lagged GDP growth:c*YR_?(-1)*dlog(V_?(-1))")

copy NIM_* NIMU_*  
call AddEquation(%blocs, "NIMU net migration;(12);" _
  + "log((1 + NIMU_?/NE_?(-1))/(1 + NIM_?(-1)/NE_?(-2))) = " _
  + "error correction:c*log(1 + NIM_?(-1)/NE_?(-2));" _
  + "momentum:c*dlog(1 + NIM_?(-1)/NE_?(-2));" _
  + "relative income:c*YR_?(-1);" _
  + "employment growth:0.02*dlog(NE_?)")

call AddEquation(%blocs, "NUR urban population;-;" _
  + "dlog(1/(1/(NUR_?/N_?)-1)) = " _
  + "GDP per capita:c*log(V_?(-1)/N_?(-1))")
  
'========= FISCAL POLICY

'--- special adjustment to government income ($pp m)
smpl %start %end
p_Bloc.genr YGADJ_? = 0
smpl %start %latest

call AddEquation(%blocs, "YG government income;-;" _
  + "dlog(1/(((1/(YG_?-YGADJ_?))*0.4*Y_?(-1)-1)*0.4*Y_?(-1))) = " _
  + "error correction:" _
    + "c*log(1/(((1/(YG_?(-1)-YGADJ_?(-1)))" _
      + "*0.4*Y_?(-2)-1)*0.4*Y_?(-2)));" _
  + "outstanding debt:0.015*log(LG_?(-1)/Y_?(-1));" _
  + "income growth:c*dlog(Y_?);" _
  + "lagged income growth:c*dlog(Y_?(-1));" _
  + "debt interest:" _
      +  "-0.1*irm_?(-1)*LG_?(-1)/(100*Y_?(-1))")

'NB: imposed debt and debt interest coefficients
' force income to increase if debt is high and make interest
'  charges subtract from government's net income

call AddEquation(%blocs, "G government spending;-;" _
  + "dlog(G_?) = " _
  + "error correction:c*log(G_?(-1));" _
  + "increase in government income:c*dlog(YG_?);" _
  + "government income:c*YG_?(-1)/Y_?(-1);" _
  + "population:c*log(N_?(-1));" _
  + "outstanding debt:c*log(LG_?(-1)/Y_?(-1));" _
  + "current account:c*CA$_?(-1)/Y$_?(-1)")

call AddEquation(%blocs, "NGI covered debt;-;" _
  + "dlog(NGI_?) = " _
  + "error correction:c*log(NGI_?(-1));" _
  + "exchange reserve:c*log(R$_?(-1)/rx_?(-1));" _
  + "income growth:c*dlog(Y_?)")

call AddEquation(%blocs, "IAGO other govt asset transactions;-R;" _
  + "IAGO_?/Y_?(-1) = " _
  + "outstanding debt:-0.2*LG_?(-1)/Y_?(-1);" _
  + "government balance:0.2*NLG_?/Y_?(-1)")

'========= PRIVATE EXPENDITURE

call AddEquation(%blocs, "SP private savings;-R;" _
  + "d(SP_?/YP_?(-1)) = " _
  + "error correction:-0.2*SP_?(-1)/YP_?(-2);" _
  + "growth of private income:c*d(YP_?)/YP_?(-1);" _
  + "wealth:-0.008*d(WP_?(-1))/WP_?(-2);" _
  + "inflation:c*spvi_?;" _
  + "short-term interest rate:0.07*irs_?(-1)/100")

'Note: imposed coefficients on the error correction term
'and the wealth term are derived from a theoretical model
'that assumes gradual adjustment of savings to achieve a
'wealth/income ratio similar to the estimated historical
'average of 5 1/2.

'The imposed coefficient on the real interest rate is the
'same as the estimated coefficient for inflation.

call AddEquation(%blocs, "pkp real asset price;-;" _
  + "dlog(pkp_?) = " _
  + "error correction:c*log(pkp_?(-1));" _
  + "momentum:c*dlog(pkp_?(-1));" _
  + "capacity utilisation:0.5*log(V_?/VT_?)")

'NB: coefficient on capacity utilisation imposed for stability

'--- flow/flow adjustment with lower bound
  call AddEquation(%blocs, "IP private investment;1;" _
  + "dlog(IP_?/V_?(-1)-0.05) = " _
  + "error correction:c*log(IP_?(-1)/V_?(-2)-0.05);" _
  + "GDP growth rate:0.5*dlog(V_?);" _
  + "bank lending:c*ILN_?(-1)/V_?(-1);" _
  + "bond rate:-0.2*irm_?/100")

'NB: imposed GDP growth and bond rate coefficients prevent unstable feedback

call AddEquation(%blocs, "IV inventory changes;-R;" _
  + "d(IV_?/V_?(-1)) = " _
  + "error correction:c*IV_?(-1)/V_?(-2);" _
  + "GDP growth rate:c*d(V_?)/V_?(-1);" _
  + "bank lending:0.05*ILN_?(-1)/V_?(-1);" _
  + "change in bank lending:0.05*d(ILN_?/V_?(-1));" _
  + "short-term interest rate:-0.0125*irs_?/100")

'Note: see above (equation for IP)

'NB: imposed bank lending coefficients prevent unstable feedback

'--- upper bound at 2.5 x national income
call AddEquation(%blocs, "NFI covered bank lending;-;" _
  + "dlog(1/(2.5/(NFI_?/Y_?(-1))-1))+4*WLNA_?/LN_?(-1) = " _
  + "error correction:c*log(1/(2.5/(NFI_?(-1)/Y_?(-2))-1));" _
  + "national income:c*log(Y_?(-1));" _
  + "growth of national income:0.3*dlog(Y_?);" _
  + "liquidity:c*NFF_?/NFI_?(-1);" _
  + "increase in liquidity:c*d(NFF_?)/NFI_?(-1);" _
  + "government debt held by banks:c*d(LGF_?)/Y_?(-1);" _
  + "reserves & covered ext position:" _
    + "c*(R$_?(-1)+NXI$_?(-1))/Y$_?(-1)")

'NB: imposed income growth coefficient prevents unstable
'    investment feedback

'========= MONETARY POLICY, RESERVES AND CAPITAL FLOWS
    
'--- ceiling; non-bank holdings must not exceed the total
call AddEquation(%blocs, _
 "LGO non-bank holdings of govt debt;-;" _
  + "dlog(1/(1/(LGO_?/LG_?)-1)) = " _
  + "non-bank liquidity:c*dlog(DP_?(-1)/Y_?(-1))")

'NB: LGO must be the first variable on the lhs

call AddEquation(%blocs, "is short-term interest rate;R(12);" _
  + "dlog(0.4+is_?/100) = " _
  + "error correction:c*log(0.4+is_?(-1)/100);" _
  + "inflation:c*log(0.4+pvi_?(-1)/100);" _
  + "rate of change in inflation:c*dlog(0.4+pvi_?/100);" _
  + "capacity utilisation:c*log(V_?/VT_?)")

'Note: a margin of 40% is allowed to ensure that numerical
'simulation for inflation and interest rates does not generate
'invalid values. So long as the same margin is used, the 
'impact of explanatory variables on dependent variables does
'not change much as inflation and interest rates rise or fall.

call AddEquation(%blocs, "im bond rate;-;" _
  + "log(0.4+im_?/100) = " _
  + "short-term rate:c*log(0.4+is_?(-1)/100);" _
  + "rate of change in short-term rate:c*dlog(0.4+is_?/100);" _
  + "inflation:c*log(0.4+pvi_?(-1)/100);" _
  + "rate of change in inflation:c*dlog(0.4+pvi_?/100)")

'Note: see note to equation for is

'--- upper bound equal to 1 x national income
call AddEquation(%blocs, "R$ exchange reserves;-;" _
  + "dlog(1/(1.0/(R$_?/(rx_?*Y_?(-1)))-1)) = " _
  + "valuation ratio:c*log(1+rpr$_?);" _
  + "current account:c*CA$_?/Y$_?(-1);" _
  + "lagged c/a:c*CA$_?(-1)/Y$_?(-1)")
  
call AddEquation(%blocs, "rpr$ reserve valuation; 1;" _
  + "log(1+rpr$_?) = " _
  + "global dollar inflation:c*dlog(ph_w)")

'--- upper bound equal to 6 x national income
call AddEquation(%blocs, "NXI$ covered external position;-;" _
  + "dlog(1/(6.0/(NXI$_?/(rx_?*Y_?(-1)))-1)) = " _
  + "income growth:c*dlog(Y_?(-1));" _
  + "income level:c*log(Y_?(-1));" _
  + "increase in exchange reserves:c*dlog(R$_?(-1));" _
  + "relative income:c*dlog(1+YR_?(-1))")

copy rpaxo$* rpaxo$u*
call AddEquation(%blocs, "rpaxo$u external asset valuation; 1;" _
  + "log(0.5+rpaxo$u_?) = " _
  + "world dollar inflation:c*dlog(ph_w)")

call AddEquation(%blocs, "rplx$ external liability valuation; 1;" _
  + "log(0.5+rplx$_?) = " _
  + "domestic asset inflation:c*dlog(pkp_?);" _
  + "world dollar inflation:c*dlog(ph_w)")

copy rx_* rxu_*
call AddEquation(%blocs, "rxu real exchange rate;-;" _
  + "log(rxu_?/rx_?(-1)) = " _
  + "error correction:c*log(rx_?(-1));" _
  + "current account:0.3*CA$_?(-1)/(NXI$_?(-1)+M$_?(-1));" _
  + "external position:0.1*NX$_?(-1)/(rx_?(-1)*Y_?(-1));" _
  + "inflation:c*d(spvi_?(-1));" _
  + "global dollar inflation:c*dlog(ph_w)")

'NB: coefficients are imposed to force adjustment in case of
'    imbalance in the current account and external position
  
'========= CAPACITY UTILISATION AND INFLATION

call AddEquation(%blocs, "pvi cost inflation;-R;" _
  + "dlog(0.04+pvi_?/100) = " _
  + "error correction:c*log(0.04+pvi_?(-1)/100);" _
  + "capacity utilisation:" _
    + "c*(1/(1.60-V_?/VT_?)+1/(1.60-V_?(-1)/VT_?(-1)));" _
  + "real exchange rate:c*dlog(rx_?);" _
  + "world oil price:c*dlog(pe_w/rx_?)")

'========= CURRENT ACCOUNT

'--- special adjustment to external transfers ($wpp m)
smpl %start %end
p_Bloc.genr BITADJ$_? = 0
smpl %start %latest

copy BIT$_* BIT$U_*
call AddEquation(%blocs, "BIT$U net income and transfers;-;" _
  + "(BIT$U_?-BIT$_?(-1)-d(BITADJ$_?))/Y$_?(-1) = " _
  + "error correction:c*(BIT$_?(-1)-BITADJ$_?(-1))/Y$_?(-1);" _
  + "external position:c*im_us*NX$_?(-1)/(100*Y$_?(-1));" _
  + "change in external position:c*d(NX$_?)/Y$_?(-1)")

copy NIT$_* NIT$U_*
call AddEquation(%blocs, "NIT$U covered income and transfers;-;" _
  + "log((1+NIT$U_?)/(1+NIT$_?(-1))) = " _
  + "error correction:c*log(1+NIT$_?(-1));" _
  + "covered position:c*log(NXN$_?(-1));" _
  + "growth of world exports:c*dlog(X$_W)")

'========= TRADE IN PRIMARY COMMODITIES

' NB  The world terms of trade responds to growth of world imports
' but does not necessarily clear the market. Therefore exports
' of each bloc are scaled to ensure that world exports will
' equal world imports and net exports of each bloc are adjusted
' accordingly.

copy BA0_* BA0U_*
call AddEquation(%blocs, _
 "BA0U net exports of primary commodities at base-year prices;-;" _
  + "(BA0U_?-BA0_?(-1))/V_?(-1) = " _
  + "world prices:c*d(lpa_?(-1));" _
  + "GDP growth:-0.05*d(V_?)/V_?(-1)")

'NB: the imposed GDP growth coefficient represents the raw material
'    content of output
  
call AddEquation(%blocs, _
 "XA0 exports of primary commodities at base-year prices; 1;" _
  + "d(XA0_?)/V_?(-1) = " _
  + "net export surplus (if any):" _
                + "c*d(@iif(BA0U_?>0,BA0U_?,0))/V_?(-1)")

call AddEquation(%blocs, _
 "MA$ imports of primary commodities; 1;" _
  + "dlog(MA$_?/MA0_?) = " _
  + "world prices:c*dlog(pa_w);" _
  + "exchange rate:c*dlog(rx_?/pp0_?)")

copy XA$_* XA$U_*
call AddEquation(%blocs, _
 "XA$U exports of primary commodities; 1;" _
  + "log(XA$U_?*XA0_?(-1)/(XA0_?*XA$_?(-1))) = " _
  + "world prices:c*dlog(pa_w);" _
  + "exchange rate:c*dlog(rx_?/pp0_?)")

'========= TRADE IN ENERGY PRODUCTS

' Energy supply, demand and trade flows are determined in 
' physical terms (see below). The following equations convert
' million tons of oil equivalent to values at prices of the
' year 2000 and then use the world price of oil to determine
' values at current prices.

copy ME0_* ME0U_*
call AddEquation(%blocs, _
 "ME0U imports of energy products at base-year prices;(12);" _
  + "log((ME0U_?/EM_?)/(ME0_?(-1)/EM_?(-1))) = " _
  + "error correction:c*log(ME0_?(-1)/EM_?(-1))")

call AddEquation(%blocs, _
 "XE0 exports of energy products at base-year prices;(12);" _
  + "dlog((1+XE0_?)*(0.01+EX_?(-1))) = " _
  + "error correction:c*log((1+XE0_?(-1))/(0.01+EX_?(-1)))")

call AddEquation(%blocs, "ME$ imports of energy products; 1;" _
  + "dlog(ME$_?/ME0_?)= " _
  + "world prices:c*dlog(pe_w)")

copy XE$_* XE$U_*
call AddEquation(%blocs, "XE$U exports of energy products; 1;" _
  + "log(((0.01+XE$U_?)/(0.01+XE0_?))" _
    + "/((0.01+XE$_?(-1))/(0.01+XE0_?(-1)))) = " _
  + "world prices:c*dlog(pe_w)")

'========= TRADE IN MANUFACTURES

call AddEquation(%blocs, "MM$ imports of manufactures;-;" _
  + "dlog(MM$_?) = " _
  + "error correction:c*log(MM$_?(-1));" _
  + "trend:c*@trend();" _
  + "demand:c*log(rx_?(-1)*(C_?(-1)+0.4*G_?(-1)" _
    + "+2*(IP_?(-1)+IV_?(-1)))+X$_?(-1)+2*XM$_?(-1));" _
  + "growth of demand:1*dlog(rx_?*(C_?+0.4*G_?" _
    + "+2*(IP_?+IV_?))+X$_?+2*XM$_?);" _
  + "change in real exchange rate:c*dlog(rx_?);" _
  + "supplier prices:c*log(pmm0_?(-1))")

'NB: demand weights represent relative import intensity
'    and the coefficient on demand growth is imposed to prevent
'    excessive import volatility in EAH

copy MM0_* MM0U_*
call AddEquation(%blocs, _
 "MM0U imports of manufactures at base-year prices;-;" _
  + "log((MM0U_?*pmm0_?/MM$_?)" _
    + "/(MM0_?(-1)*pmm0_?(-1)/MM$_?(-1))) = " _
  + "error correction:c*log(MM0_?(-1)*pmm0_?(-1)/MM$_?(-1));" _
  + "supplier price change:c*dlog(pmm0_?);" _
  + "exchange rate appreciation:c*dlog(rx_?)")

'--- market shares for exports of manufactures

'--- estimation using a single panel with data for
'    supplier-partner pairs having 5% or more of
'    the partner's import market
%l = ""
for !j = 1 to nBloc
  %p = t_Bloc(!j, 1)
  for !i = 1 to nBloc
    %b = t_Bloc(!i, 1)
    %s = "sxm_" + %b + "_"+ %p
    if @isobject(%s) then
      if @elem({%s}, %base) >= 0.05 _
        and @min({%s}) > 0.001 then
        %l = %l + %b + "_" + %p + " "
        series ucx$_{%b}_{%p} = ucx$_{%b}
      endif
    endif
  next
next

call AddEquation(%l, _
 "sxm manufactured export market shares;(12);" _
  + "dlog(sxm_?) = " _
  + "error correction:c*log(sxm_?(-1));" _
  + "unit cost:c*log(ucx$_?(-1));" _
  + "increase in unit cost:c*dlog(ucx$_?(-1))")

call AddEquation(%blocs, _
 "XM0 exports of manufactures at base-year prices;-;" _
  + "dlog(XM0_?/XM$_?) = " _
  + "error correction:c*log(XM0_?(-1)/XM$_?(-1));" _
  + "unit cost:c*log(ucx$_?(-1))")

'========= TRADE IN SERVICES

' Service trade is balanced by adjusting exports of
' each bloc to ensure that world exports are equal to
' world imports (market share basis).

copy BS$_* BS$U_*
call AddEquation(%blocs, "BS$U net exports of services; 1;" _
  + "(BS$U_?-BS$_?(-1))/V_?(-1) = " _
  + "real exchange rate appreciation:c*dlog(rx_?);" _
  + "increase in net exports of raw materials:c*d(BA$_?)/V_?(-1);" _
  + "increase in net exports of energy:c*d(BE$_?)/V_?(-1);" _
  + "increase in net exports of manufactures:c*d(BM$_?)/V_?(-1)")

call AddEquation(%blocs, "MS$ imports of services; 1;" _
  + "d(MS$_?)/V_?(-1) = " _
  + "net imports (if positive):" _
               + "c*d(@iif(BS$U_?>0,0,-BS$U_?))/V_?(-1);" _
  + "real exchange rate appreciation:c*dlog(rx_?);" _
  + "increase in imports of raw materials:c*d(MA$_?)/V_?(-1);" _
  + "increase in exports of energy:c*d(XE$_?)/V_?(-1);" _
  + "increase in imports of manufactures:c*d(MM$_?)/V_?(-1)")

copy MS0_* MS0U_*
call AddEquation(%blocs, _
 "MS0U imports of services at base-year prices;-;" _
  + "log(MS0U_?*MS$_?(-1)/(MS$_?*MS0_?(-1))) = " _
  + "error correction:c*log(MS0_?(-1)/MS$_?(-1));" _
  + "exchange rate appreciation:c*dlog(rx_?)")

call AddEquation(%blocs, _
 "XS0 exports of services at base-year prices;-;" _
  + "dlog(XS0_?/XS$_?) = " _
  + "error correction:c*log(XS0_?(-1)/XS$_?(-1));" _
  + "exchange rate appreciation:c*dlog(rx_?)")

'========= PHYSICAL ENERGY
' Global supply/demand balance is achieved by adjusting the
' world price of oil.

call AddEquation(%blocs, "ED primary energy demand;-;" _
  + "dlog(ED_?/N_?) = " _
  + "error correction:c*log(ED_?(-1)/N_?(-1));" _
  + "per capita GDP growth:c*dlog(V_?/N_?);" _
  + "change in user energy price:c*d(lped_?);" _
  + "terms of trade change:c*dlog(tt_?);" _
  + "export growth:c*dlog(X0_?);" _
  + "inventory change:c*IV_?/V_?(-1);" _
  + "per capita income:c*log(1+YR_?(-1))")

call AddEquation(%blocs, "EPN primary non-carbon energy production;-;" _
  + "dlog(EPN_?/@movav(ED_?(-1),4)) = " _
  + "error correction:c*log(EPN_?(-1)/@movav(ED_?(-2),4));" _
  + "short-run variation in energy demand:c*dlog(ED_?);" _
  + "cost of carbon energy:0.1*lpepc_?(-1)")

call AddEquation(%blocs, "EPC primary carbon energy production;-;" _
  + "dlog(EPC_?) = " _
  + "domestic demand:c*dlog(ED_?)*" _
    + "@iif(ED_?(-1)>EP_?(-1),1," _
      + "@iif(ED_?(-1)>EPN_?(-1)," _
        + "(ED_?(-1)-EPN_?(-1))/EPC_?(-1),0));" _
  + "change in producer price:0.1*d(lpep_?)")

'--- energy imports
' NB normalisation: imports in excess of the amount required
' required to satisfy shortfalls in domestic supply (if any)

call AddEquation(%blocs, "EM primary energy imports; -;" _
  + "dlog(0.01+EM_?-@iif(ED_?>EP_?,ED_?-EP_?,0)) = " _
  + "error correction:c*log(0.01+EM_?(-1)" _
    + "-@iif(ED_?(-1)>EP_?(-1),ED_?(-1)-EP_?(-1),0));" _
  + "growth of world production:c*dlog(EP_W)")

'--- CO2 emissions
'    provision for impact of carbon tax (lttco2)
call AddEquation(%blocs, "CO2 CO2 emissions; -;" _
  + "dlog(CO2_?/(ED_?-EPN_?))+0.1*lttco2_? = " _
  + "error correction:c*log(CO2_?(-1)/(ED_?(-1)-EPN_?(-1)));" _
  + "per capita income:c*log(YN_?(-1))")

'========= WELL-BEING INDICATORS
call AddEquation(%blocs, "JHD human development index;(1);" _
  + "dlog(JHD_?) = " _
  + "error correction:c*log(JHD_?(-1));" _
  + "momentum:c*dlog(JHD_?(-1));" _
  + "per capita income:c*1/YN_?(-1)^0.8;" _
  + "income growth:c*dlog(YN_?)")

  call AddEquation(%blocs, "JGN internal Gini index;(1);" _
  + "dlog(JGN_?) = " _
  + "error correction:c*log(JGN_?(-1));" _
  + "momentum:c*dlog(JGN_?(-1));" _
  + "income growth:c*log(YN_?/YN_?(-4));" _
  + "government services:c*log(G_?(-1)/V_?(-1))")

call AddEquation(%blocs, "JIM infant mortality rate;(1);" _
  + "dlog(JIM_?) = " _
  + "error correction:c*log(JIM_?(-1));" _
  + "momentum:c*dlog(JIM_?(-1));" _
  + "per capita income:c*1/YN_?(-1)^0.4;" _
  + "income growth:c*dlog(YN_?)")

call AddEquation(%blocs, "JLX life expectancy at birth;(1);" _
  + "dlog(JLX_?) = " _
  + "error correction:c*log(JLX_?(-1));" _
  + "momentum:c*dlog(JLX_?(-1));" _
  + "income growth:c*log(YN_?/YN_?(-4));" _
  + "per capita income:c*1/YN_?(-1)^0.4;" _
  + "trend:c*@trend()")

'==================================================================
' PROCESSING
'==================================================================

!qList = @upper(@left(%equation_listing,1)) = "Y"
!qCompare = @upper(@left(%comparisons,1)) = "Y"
!qGraph = @upper(@left(%prediction_graphs,1)) = "Y"

'============ estimation header
call pLog("estimating the equations")

'--- equation listing table
table(1,10) t_QL
t_QL.setwidth(1) 5
t_QL.setwidth(2) 12
t_QL.setwidth(3:9) 8
t_QL.setwidth(10) 12

if !qCompare then
  '--- readme documenting estimation models
  pageselect tables
  table(8,1) readme
  %s = "Behavioural equations for " + %sysTitle + _
      " estimated on data for " + %startest + "-" + %endest
  setcell(readme,1,1,%s,"l")
  %s =  "Results are provided for the following specifications:"
  setcell(readme,3,1, %s,"l")
  setcell(readme,5,1, "  1: common intercept, cross-section weights","l")
  setcell(readme,6,1, "  2: cross-section intercepts, cross-section weights","l")
  setcell(readme,7,1, "  3: common intercept, cross-section SUR","l")
  setcell(readme,8,1, "  4: cross-section intercepts, cross-section SUR","l")
  
  '--- estimation results table
  pageselect data
  table(20,10) tcomp
endif

'========= process each equation
for !i = 1 to nEq
  %list = t_Eq(!i,1)
  %var = t_Eq(!i,2)
  statusline Equation %var
  %desc = t_Eq(!i,3)
  %desc = @upper(@left(%desc,1)) + @mid(%desc,2)
  %spec = t_Eq(!i,4)
  !npass = 5
  %tfit = ""
  '--- option to estimate intercepts and AR(1) on recent data only
  if @instr(%spec, "R") <> 0 then
    !npass = 6
    %tfit = "intercepts and ar(1) estimated on data for " _
    + %startfit + "-" + %latest
    %spec = @replace(%mode,"R","")
  endif
  '--- default specification is type 2
  if %spec = "-" or %spec = "" then %spec = "2" endif
  %tlspec = t_Eq(!i,5)
  %eq = t_Eq(!i,6)
  '--- create the pool for the equation
  pool p_{%var} {%list}

  '--- create instrument variable (except eqn for sxm)
  if %var <> "sxm" then
    smpl %start %end
    for !j = 1 to nBloc
      %b = t_Bloc(!j, 1)
      series {%var}_{%b}_ins = 0
    next
  endif

  '--- headings for comparison table
  if !qCompare then
    setcell(tcomp,2,2, "Full sample (unrestricted)","c")
    if !npass = 6 then
      setcell(tcomp,1,6, "Model version","c")
      setcell(tcomp,2,6, "full","c")
      setcell(tcomp,3,6, "sample","c")
      setcell(tcomp,2,7, "reduced","c")
      setcell(tcomp,3,7, "sample","c")
      tcomp.setmerge(1,6,1,7)
    else
      setcell(tcomp,2,6, "Model version","c")
    endif  
    tcomp.setmerge(2,2,2,5)
    
    '--- starting row for results
    !icomp0 = 4
    '--- if comparison requested, estimate all specifications
    !iem0 = 1
  else
    'otherwise do pass 5 and 6 only
    !iem0 = 5
  endif

  '--- loop to repeat estimation
  ' passes 1-4 are for alternative specifications
  ' pass 5 is for the selected spec, full sample
  ' but may have some coefficients imposed
  ' pass 6 re-estimates intercepts and AR(1) on the reduced sample

  '--- estimation sample
  smpl %startest %endest
  delete p_QL*

  for !iem = !iem0 to !npass
    '--- reduced sample for pass 6
    if !iem = 6 then smpl %startfit %latest endif

    '--- check restricted list of variants
    if !iem > 4 or %tlspec = "" or _
                   @instr(%tlspec, @str(!iem)) > 0 then
      call PoolEstimation(%var, !iem, p_{%var}, %spec, %eq)
      '--- save a copy for listings and predicted value calculation
      if !iem = 5 then copy p_{%var} p_QL endif
      '--- generate the equation listing at the final pass
      if !qList then
        if !iem = !npass then
          '--- check whether there are fixed effects
          !q = 0
          if @instr("24", %spec) > 0 then !q = 1 endif
          !nQL = 1
          call ListEq(t_QL, !nQL, %var, %desc, %eq, !q, _
            p_QL, p_{%var}, %tfit)
          copy t_QL teq_{%var}
          t_QL.deleterow(1) !nQL
        endif
      endif
      '--- write the comparison table (every pass)
      if !qCompare then
        !ncomp = !icomp0
        call SaveStats(tcomp, !ncomp, %desc, !iem, p_{%var}, %spec, %eq)
      endif
    endif
  next
  if !qCompare then
    copy tcomp tcomp_{%var}
    tcomp.deleterow(1) !ncomp
  endif

  '--- build actual/predicted graph
  if !qGraph then
    '--- check whether dependent variable is an unadjusted variant
    '    and if so, modify the equation to use lagged values of the
    '    unadjusted variable in historical predictions
    call Adjusted(%var, %v)
    if %v <> %var and @instr(%eq, %v+"_?") > 0 then
      smpl %startest %endest
      %eqrev = @replace(%eq, %v+"_?", %var+"_?")
      call PoolEstimation(%var, 5, p_QL, %spec, %eqrev)
    endif
  
    '--- generate predicted values for the historical period
    model m_fit
    m_fit.append :p_QL
    m_fit.scenario(n,a=f) fitted
    '--- drop 8 observations (lagged dependent variable)
    %first = @str(@val(%start)+8)
    smpl %first %latest
    m_fit.solve
    delete m_fit
  
    '--- make a bloc graph showing predicted and actual values
    %ss = %var + "_?_f " + %var + "_?"
    %tt = "Actual (blue), Predicted (red)"
    '--- number of columns in composite graph display
    !n = 6
    if %var = "sxm" then !n = 7 endif
    call BlocGraph("grf_" + %var, %desc, %list, %ss, %tt, "", "", _
      !n, %first, %latest, %latest)
    delete sgrb*
  endif
next

'======== SPECIAL ESTIMATION ==================

'--- world price of primary commodities
' log normalisation
smpl %start %end
series pa_w_ins = 0
smpl %startest %endest
%lhs = "dlog(pa_w)-pa_w_ins"
equation eq_pa_w.ls {%lhs} _
  log(pa_w(-1)) log(V_W(-1)) dlog(V_W) c ar(1)

'--- reestimate intercept and ar
'smpl %startfit %endest
'%s = @str(eq_pa_w.@coefs(1)) + "*log(pa_w(-1))" _
'   + "+" + @str(eq_pa_w.@coefs(2)) + "*log(V_W(-1))"
'%s = %lhs + "-(" + %s + ") c ar(1)"
'eq_pa_w.ls {%s}

'--- market shares for exports of manufactures
smpl %start %end

'--- estimated structural coefficients
%c1 = @str(p_sxm.@coefs(2))
%c2 = @str(p_sxm.@coefs(3))
%c3 = @str(p_sxm.@coefs(4))

'--- loop over suppliers
for !j = 1 to nBloc
  %b = t_Bloc(!j, 1)
  '--- instrument to adjust supplier shares in all markets
  %s = "sxmu_" + %b + "_ins"
  if not @isobject(%s) then
    series {%s} = 0
  endif  

'--- for each supplier, build a list of partners for which series
'    are available (when small blocs or individual countries are
'    used, some bilateral series may be missing).

'--- loop over partners to build the list and create series with
'    unadjusted shares sxmu
  %l = ""
  for !i = 1 to nBloc
    %p = t_Bloc(!i, 1)
    %s = "sxm_" + %b + "_"+ %p
    if @isobject(%s) then
      %l = %l + " " + %p
      series sxmu_{%b}_{%p} = {%s}
      '--- instrument to control bilateral market share
      %s = "sxmu_" + %b + "_"+ %p + "_ins"
      if not @isobject(%s) then
        series {%s} = 0
      endif
    endif
  next

'--- define a pool of partners for the supplier
  pool p_sxmu_{%b} {%l}

'--- predict each unadjusted market share
'    (results will be scaled to sum to 1)

  %s1 = "sxm_" + %b + "_?(-1)"
  %s = "log(sxmu_" + %b + "_?)-log(" + %s1 + ")" _
    + "-sxmu_" + %b + "_ins" _
    + "-sxmu_" + %b + "_?_ins-(" _
    + %c1 + "*log(" + %s1 + ")+" _
    + %c2 + "*log(ucx$_" + %b + "(-1))+" _
    + %c3 + "*dlog(ucx$_" + %b + "(-1)))"

  smpl %startfit %endest
  p_sxmu_{%b}.ls(cx=f,wgt=cxdiag) {%s} c ar(1)
  smpl %start %end

next

'======== CLEAN UP ================================

smpl %start %latest

'--- remove supplier-partner unit costs
'    used to estimate the response of mf export market shares
delete ucx$_*_*
delete t_QL

pageselect tables
copy data\teq_*
copy data\tcomp_*

pageselect graphs
copy data\grf_*

pageselect data
delete teq_* tcomp_* grf_* *_f

call pEnd
endsub

subroutine AddEquation(string %List, string %Def)
'==================================================
'add an equation to table t_Eq
'
' Call: %List    list of pool members
'       %Def     definition
'
' Ret:
'
' Modifies  nEq  equation count
'           tEq  array of equation definitions
'
' Note: the equation definition must be supplied
'       in the format
'
'   var description; options; lhs = c*term ; ...
'
' Options include
'  -    use the default estimation variant (model 2)
'  1-4  use a given variant 1..4
'  R    use recent data to fit intercepts and AR(1)
'  (k..)  restricted list of variants to be estimated (in case
'         some models break down or do not converge)
'
' Columns of the table store the following information
'
'   1  list of pool members for which the equation will be
'      estimated. if none, an EViews equation object will be
'      used instead of a pool.
'
'   2  root name of the variable which the equation determines
'      the root name does not include a bloc suffix _? or U
'      (designator for unadjusted version of a variable subject
'      to adjustment)
'
'   3  description of the variable
'
'   4  estimation variant to be used in the model (1-4)
'      with suffix R if intercepts and AR(1) are estimated
'      on recent data only
'
'   5  if not blank, list of variants to be estimated
'
'   6  the equation written as lhs = rhs
'      where the first variable in the lhs expression is the
'      dependent variable (includes U if the variable will be
'      adjusted, and _? if used with a pool)
'      the rhs is a list of terms separated by semi-colon ;
'      each rhs term takes the form       coef*expression
'      where coef is a number (imposed value) or c if the value
'      of the coefficient is to be estimated
'
'  Related library routines
'
'  FindEqn(Var,iEq)  returns the equation number determining Var
'                    where Var is the root name of a variable
'                    (returns 0 if Var is not determined by a
'                    behavioural equation)
'
'  LHName(iEq, Name) returns the name of the dependent variable
'                    in equation iEq as it appears on the lhs
'                    of the equation (including _? and U
'
'---------------------------------------------------------------

nEq = nEq + 1

'--- list of pool members
t_Eq(nEq, 1) = %List
%lib_eq = %Def

'--- variable name and description
call Token(%lib_eq, ";", %lib_s)
call Token(%lib_s, " ", %lib_v)
t_Eq(nEq, 2) = %lib_v
t_Eq(nEq, 3) = %lib_s

'--- model or model (spec list)
call Token(%lib_eq, ";", %lib_s)
call Token(%lib_s, "(", %lib_m)
if %lib_s = "" then
  %lib_tl = ""
else
  call Token(%lib_s, ")", %lib_tl)
endif 
t_Eq(nEq, 4) = %lib_m
t_Eq(nEq, 5) = %lib_tl

'--- rhs = lhs
t_Eq(nEq, 6) = %lib_eq

endsub

subroutine local ListEq(table tQL, scalar nRow, _
  string %Var, string %Desc, string %Eq, _
  scalar qFE, pool p1, pool p2, string %Fit)
'==================================================
'add listing for an estimated equation to table
'
' Call: tQL   table with equation listings (cols 1-9)
'       nRow  number of rows
'       %Var  variable name
'       %Desc description
'       %Eq   equation definition
'       qFE   fixed effects flag
'       p1    pool with coefficient estimates
'       p2    pool with intercepts and AR(1) stats
'       %Fit  text to display if intercepts and AR(1)
'             have been estimated on recent data,
'             blank otherwise
'
' Ret:  tQL   updated table
'       nRow  updated number of rows
'
'---------------------------------------------------------------

%rhs = @replace(@replace(%Eq,"_?",""),"@iif","if")
call Token(%rhs, "=", %lhs)
!i = nRow + 1

'--- name and description of lhs variable
setcell(tQL,!i,1,%Var,"c")
setcell(tQL,!i,2,%Desc+"    " + %lhs + "","l")

'--- unit root test for lhs variable
%t = %Eq
call Token(%t, "=", %lhs)
!vstat = 0
!vprob = 0
call z_PoolUnitroot(p1, %lhs, !vstat, !vprob)

!i = !i + 1
setcell(tQL,!i,8,"value","c")
setcell(tQL,!i,9,"probability","c")
!i = !i + 1
setcell(tQL,!i,2,"unit root test","l")
setcell(tQL,!i,6,"Fisher ADF","c")
%merg = @str(!i)+",6,"+@str(!i)+",7"
tQL.setmerge({%merg})
setcell(tQL,!i,8,!vstat,"c",1)
setcell(tQL,!i,9,!vprob,"c",3)

'--- estimated structural coefficients and t-stats
!nest = 1
!i = !i + 2
setcell(tQL,!i,2,"coefficients","l")
setcell(tQL,!i,6,"coeff","r")
setcell(tQL,!i,7,"t-stat","c")
setcell(tQL,!i,8,"description","l")
%merg = @str(!i)+",8,"+@str(!i)+",9"
tQL.setmerge({%merg})
!i = !i + 1

'--- for each rhs term
call Token(%rhs, ";", %term)
while %term <> ""
  '--- description of the term
  if @instr(%term, ":") > 0 then
    call Token(%term, ":", %t)
  else  
    %t = ""
  endif  
  setcell(tQL,!i,8,%t,"l")
  %merg = @str(!i)+",8,"+@str(!i)+",9"
  tQL.setmerge({%merg})
  '--- coefficient and t-stat
  call Token(%term, "*", %coef)
  if %coef = "c" then
    !nest = !nest + 1
    setcell(tQL,!i,6,p1.@coefs(!nest),"r",3)
    setcell(tQL,!i,7,"("+@str(@abs(p1.@tstats(!nest)),"f.1")+")","c")
  else 
    setcell(tQL,!i,6,@val(%coef),"r",3)
  endif  
  '--- expression
  if @len(%term) > 36 then
    %term = @left(%term,34) + "..."
  endif  
  setcell(tQL,!i,5,%term,"r")
  %merg = @str(!i)+",5,"+@str(!i)+",2"
  tQL.setmerge({%merg})
  !i = !i + 1
  call Token(%rhs, ";", %term)
wend

'--- estimated intercepts and auto-correlation
!i = !i + 1
if %Fit <> "" then
  setcell(tQL,!i,2,%Fit,"l")
  !i = !i + 2
endif
setcell(tQL,!i,2,"statistics","l")
setcell(tQL,!i,4,"value","r")
setcell(tQL,!i,5,"t-stat","c")
setcell(tQL,!i,8,"value","r")
setcell(tQL,!i,9,"t-stat","c")
!i = !i + 1
setcell(tQL,!i,3,"constant","r")
setcell(tQL,!i,4,p2.@coefs(1),"r",3)
setcell(tQL,!i,5,"("+@str(@abs(p2.@tstats(1)),"f.1")+")","c")
setcell(tQL,!i,6,"residual ar(1)","c")
%merg = @str(!i)+",6,"+@str(!i)+",7"
tQL.setmerge({%merg})
setcell(tQL,!i,8,p2.@coefs(2),"r",3)
setcell(tQL,!i,9,"("+@str(@abs(p2.@tstats(2)),"f.1")+")","c")

'--- std error of equation
!i = !i + 1
setcell(tQL,!i,3,"se","r")
setcell(tQL,!i,4,p2.@se,"r",3)
'--- adjusted R2
'the displayed figure is meaningless because it is
'calculated on an equation that explains the structural
'residual, not the original lhs expression
'--- do not display
'setcell(tQL,!i,6,"adjusted R2","c")
'%merg = @str(!i)+",6,"+@str(!i)+",7"
'tQL.setmerge({%merg})
'setcell(tQL,!i,8,p2.@rbar2,"r",3)
'--- blank line
!i = !i+1

'--- fixed effects
if qFE > 0 then
  table tFE
  !nmem = p2.@ncrossest
  vector(!nmem) vFE
  for !j = 1 to !nmem
    tFE(!j,1) = p2.@idnameest(!j)
    vFE(!j) = p2.@effects(!j)
  next
  for !j = 1 to !nmem
    'assume this coef is the largest and look for larger
    !j2 = !j
    !v = vFE(!j)
    for !j1 = !j+1 to !nmem
      !v1 = vFE(!j1)
      '--- larger coef found
      if !v1 > !v then
        !j2 = !j1
        !v = !v1
      endif
    next
    '--- if this coef is not the largest, swap
    if !j2 <> !j then
      %s = tFE(!j,1)
      tFE(!j,1) = tFE(!j2,1)
      tFE(!j2,1) = %s
      !v1 = vFE(!j)
      vFE(!j) = !v
      vFE(!j2) = !v1
    endif
  next
  '--- write out in columns 2-9
  !i = !i + 1
  setcell(tQL,!i,2,"fixed effects","l")
  !i = !i + 1
  !ic = 2
  for !j = 1 to !nmem
    !ic1 = !ic + 1
    setcell(tQL,!i,!ic,tFE(!j,1),"r")
    '--- display with 8 significant chars incl sign
    setcell(tQL,!i,!ic1,vFE(!j),"r",-8)
    '--- if the displayed value is very small
    '    show with 5 dps
    if @instr(tQL(!i,!ic1),"E-") > 0 then
      setcell(tQL,!i,!ic1,vFE(!j),"r",5)
    endif  
    if !ic < 8 then
      !ic = !ic + 2
    else
      !i = !i + 1
      !ic = 2
    endif
  next
  delete tFE vFE
endif

nRow = !i
endsub

subroutine z_PoolUnitroot(pool p, string %Var, _
  scalar vStat, scalar vProb)
'==================================================
' perform panel Fisher ADF unit root test
'
' Call: p       pool object
'       %Var    test expression with embedded ?
'
' Ret:  vStat   statistic
'       vProb   probability of null hypothesis (unit root)
'
' Uses: %startest  start of sample period
'       %endest    end of sample period
'
'---------------------------------------------------------------
p.genr lib_s_? = {%Var}
smpl %startest %endest
freeze(lib_t) p.uroot(dif=0,ADF) lib_s_?

for !lib_i = 10 to 100
  if lib_t(!lib_i, 1) = "ADF - Fisher Chi-square" then
    vStat = @val(lib_t(!lib_i, 4))
    vProb = @val(lib_t(!lib_i, 5))
    exitloop 
  endif
next
delete lib_s_* lib_t
endsub

subroutine local PoolEstimation(string %Var, scalar iSpec, _
  pool p, string %Use, string %Eq)
'==============================================================
'use a pool object to estimate an equation
'
' Call: %Var    variable name (source for instrument name)
'       iSpec   estimation specification
'                 1-4 to estimate for alternative models
'                 5 to estimate final coefficients with full sample
'                   and include an instrument
'                 6 to re-estimate intercepts with reduced sample
'                   and include an instrument
'       p       pool object
'       %Use    specification retained for the model
'       %Eq     equation to be estimated
'
' Ret:
'
' Note: if iSpec > 4 the equation is estimated using
'       the spec specified in %spec
'       instruments are created for all vars except "sxm"
'
'---------------------------------------------------------------

%teq = %Eq

'--- strip out the lhs
call Token(%teq, "=", %lhs)

'--- loop for terms on the rhs
%rhs = ""
%lhext = ""
!ncoef = 0
!nest = 1
while 1
  call Token(%teq, ";", %term)
  if %term = "" then exitloop endif
  '--- text description of the term
  if @instr(%term, ":") > 0 then
    call Token(%term, ":", %desc)
  endif
  '--- decompose into coef * expr
  %expr = %term
  call Token(%expr, "*", %coef)
  '--- if re-estimating intercepts and ar only
  '    and the coef is a dummy, add to the lhs
  '    extension with estimated coef value from the pool
  if iSpec > 5 and %coef = "c" then
    if !ncoef > 0 then
      %lhext = %lhext + "+"
    endif
    !nest = !nest + 1
    %coef = @str(p.@coefs(!nest))
    %lhext = %lhext + "(" + %coef + "*" + %expr + ")"
    !ncoef = !ncoef + 1
  '--- if doing final estimation (passes 5+) and a
  '    coef value is supplied, add to lhs extension
  '    with the supplied value 
  else if iSpec > 4 and %coef <> "c" then
    if !ncoef > 0 then
      %lhext = %lhext + "+"
    endif
    %lhext = %lhext + "(" + %term + ")"
    !ncoef = !ncoef + 1
  '--- otherwise add the expr to the rhs list of
  '    terms whose coefficients are estimated
  else
    %rhs = %rhs + " " + %expr
  endif
  endif
wend

'--- if iSpec = 5 or 6 include an instrument on the lhs
if iSpec > 4 and %Var <> "sxm" then
  %lhs = %lhs + "-" + %Var + "_?_ins"
endif

'--- if doing final estimation (passes 5+)
'    and rhs terms have been found, add the
'    extension list to the lhs with a leading minus
'    and space chars removed
if iSpec > 4 and !ncoef > 0 then
  %lhs = %lhs + "-(" + @replace(%lhext," ","") + ")"
endif

'--- if doing final estimation (passes 5+)
'    use the specified estimation model 
if iSpec > 4 then
  !i = @val(%Use)
else
  !i = iSpec
endif
call z_ModelInfo(!i, %s1)

'--- execute the pool estimation
%s = %lhs + " c " + %rhs + " ar(1)"
call z_PoolEstimate(p, %s1, %s)

endsub

subroutine local z_ModelInfo(scalar iSpec, string %Opt)
'===============================================================
' return estimation parameters for model
'
' Call: iSpec  specification 1 to 4
'
' Ret:  %Opt   estimation options
'
'---------------------------------------------------------------

if iSpec = 1 then
  %Opt = "wgt=cxdiag,cov=cxwhite"
else if iSpec = 2 then
  %Opt = "cx=f,wgt=cxdiag,cov=cxwhite"
else if iSpec = 3 then
  %Opt = "wgt=cxsur,cov=cxwhite"
else
  %Opt = "cx=f,wgt=cxsur,cov=cxwhite"
endif
endif
endif
%Opt = %Opt + ",iter=seq"

endsub

subroutine z_PoolEstimate(pool p, string %Opt, _
 string %Eq)
'==================================================
' perform pool regression
'
' Call: p     pool object
'       %Opt  options
'       %Eq   equation to estimate
'
' Ret: updated pool object
'
'---------------------------------------------------------------

p.ls({%Opt}) {%Eq}

endsub

subroutine local SaveStats(table tStat, scalar iSRow, _
  string %Desc, scalar iSpec, pool p, string %Use, _
  string %Eq)
'======================================================
'Save estimation statistics in a display table
'
' Call: tStat   display table
'       iSRow   starting table row
'       %Desc   description of dependent variable
'       iSpec   specification number 1-6
'       p       pool object with estimation results
'       %Use    estimation options used 1-4
'       %Eq     equation definition
'
' Ret:
'
'---------------------------------------------------------------

%teq = @replace(%Eq, "_?", "")

if iSpec < 5 then
  %spec = @str(iSpec)
else
  %spec = %Use
endif

call Token(%teq, "=", %lhs)
%teq = "c*constant;" + %teq + ";c*ar(1)"

'--- headers (write at first pass)
if iSpec = 1 then
  !irow = iSRow
  tStat(!irow,1) = %Desc _
    + ":  " + %lhs
  tStat(!irow+2,1) = "Estimation method"
  tStat(!irow+3,1) = "  No of observations"
  tStat(!irow+4,1) = "  Durbin-Watson"
  tStat(!irow+5,1) = "  Standard error of equation"
  tStat(!irow+6,1) = "  Adjusted R2"
  tStat(!irow+7,1) = "  F statistic"
  !irow1 = iSRow + 8
  tStat(!irow1,1) = "Coefficients"
  tStat.setjust(!irow,1,!irow1,1) left
  tStat.setwidth(1) 20
endif


'--- estimation statistics
!irow = iSRow + 2
!icol = 1 + iSpec
tStat(!irow, !icol) = %spec
!irow = !irow + 1
!nobs = p.@regobs*p.@ncrossest
setcell(tStat,!irow,!icol,!nobs,0)
!irow = !irow + 1
setcell(tStat,!irow,!icol,p.@dw,3)
!irow = !irow + 1
setcell(tStat,!irow,!icol,p.@se,3)
!irow = !irow + 1
setcell(tStat,!irow,!icol,p.@rbar2,3)
!irow = !irow + 1
setcell(tStat,!irow,!icol,p.@f,1)

'--- coefficients
!ncoef = 0
!nest = 0
while 1
  call Token(%teq, ";", %term)
  if @instr(%term, ":") > 0 then
    call Token(%term, ":", %s)
  endif  
  if %term = "u" or %term = "" then
    exitloop
  endif
  call Token(%term, "*", %coef)
  !ncoef = !ncoef + 1
  !irow1 = iSRow + 7 + 2*!ncoef
  !irow2 = 1+!irow1
  if iSpec = 1 then
    tStat(!irow1, 1) = "  " + %term
    tStat.setjust(!irow1,1) left
  endif
  if iSpec = 6 and !ncoef > 1 and %term <> "ar(1)" then
    tStat(!irow1, !icol) = tStat(!irow1, !icol-1)
  else if iSpec > 4 and %coef <> "c" then
    tStat(!irow1, !icol) = %coef
  else
    !nest = !nest + 1
    tStat(!irow1, !icol) = p.@coefs(!nest)
    tStat.setformat(!irow1, !icol) f.3
    %s = "(" + @str(p.@stderrs(!nest), "f.3") + ")"
    tStat(!irow2, !icol) = %s
  endif
  endif
wend

iSRow = iSRow + 10 + 2*!nCoef

endsub
