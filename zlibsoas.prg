'PROGRAM: zlibsoas.prg          Copyright (C) 2011 Alphametrics Co. Ltd.
'
' CAM version 4.6  AUGUR
'
' Library routines used by SOAS
'
'---------------------------------------------------------------
'

mode quiet
'tic
'include "set"
include "zlibp"

' Use as in following examples:
'call mk_soas_graphs("e1b; Austerity scenario: e2f; Federal scenario")
'call mk_soas_graphs("e2f; Federal scenario")

'call mk_soas_graphs("e1; Struggling on + r. gov: e2; R. gov. + EU br. p: e3; MS Eur + regionalisation: e4a; Fed. Eur. + multi-polar")
'call mk_soas_graphs("e1b; SOAS baseline (IAGO=0): e2d; expansion within EZ (IAGO=0)")

' -- SUBROUTINES


subroutine mk_soas_graphs(string %p_scenario_list)
'==================================================================
' Call: %p_scenario_list    list of scenario codes and names, e.g.
' "e1; austerity: e2; federalism"

call pLog("Generating SOAS presentation graphs")

%p_page_name="soas"

pagecreate(page={%p_page_name}) a %start %end
pageselect data

call mk_single_graphs(%p_scenario_list, %p_page_name)
endsub


subroutine mk_single_graphs(string %p_tlScenario, string %p_Page)
'==================================================================
' -- Generate sequence of graphs, each on a single pane
' Call: %p_scenario_list    list of scenario codes and names
'       %p_Page             name of workfile page to save graphs onto

'%p_tlyear = "1990 2030 2013"
%p_tlyear = "1980 2030 2014"

%font = "28"
%color = %blyellow


' -- Europe
%p_grp_code = "EU"
%bloc_list = "EUN EUW EUS EUE UK"

'-- GDP and components

call SPGraph(%p_Page, %p_grp_code + "_gdp_growth", _
  "GDP growth" _
    + "\n(%)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;DV_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_consumption", _
  "consumption" _
    + "\n(% GDP)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;CV_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_investment_private", _
  "investment" _
    + "\n(% GDP)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";10,30;0;IPV_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_investment_growth", _
  "investment growth" _
    + "\n(% pa)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;@dlog(IPV_?):" _
  )

call SPGraph(%p_Page, %p_grp_code + "_current_acct", _
  "current account balance" _
    + "\n(% GDP)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;CAV$_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_current_acct_trade", _
  "trade balance" _
    + "\n(% GDP)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;100*TB$_?/(V_?*rx_?):" _
  )

call SPGraph(%p_Page, %p_grp_code + "_govt_expenditure", _
  "government spending" _
    + "\n(% GDP)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;GV_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_govt_exp_growth", _
  "growth in government spending" _
    + "\n(% pa)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;@dlog(G_?):" _
  )
'-- Govt finance

series YGADJ_EUW_e1b = 0
series YGADJ_EUN_e1b = 0
series YGADJ_EUS_e1b = 0
series YGADJ_EUE_e1b = 0
series YGADJ_UK_e1b = 0

call SPGraph(%p_Page, %p_grp_code + "_govt_income", _
  "government income" _
    + "\n(% GDP)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;YGV_?:" _
  )


call SPGraph(%p_Page, %p_grp_code + "_govt_inc_fed", _
  "government income" _
    + "\n(% GDP)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;YGV_? | (YG_?-YGADJ_?)*100/VV_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_govt_inc_exp", _
  "government income and expenditure" _
    + "\n(% GDP)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;YGV_? govt income|GV_? govt expenditure:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_govt_deficit", _
  "government net lending" _
    + "\n(% GDP)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;NLGV_?:" _
  )


call SPGraph(%p_Page, %p_grp_code + "_govt_deficit_abs", _
  "government net lending" _
    + "\n(% GDP)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;NLG_?:" _
  )



'call SPGraph(%p_Page, %p_grp_code + "_federal_debt", _
'  "European Federal debt" _
'    + "\n(% EU GDP)", _
'  %color, %font,%bloc_list,"e2f; Federal Europe",%p_tlyear,3, _
'  ";;0;@nan(100*(eu_lg+eu_lg2)/eu_y$,0):" _
'  )

call SPGraph(%p_Page, %p_grp_code + "_federal_deficit", _
  "European Federal deficit" _
    + "\n(% EU GDP)", _
  %color, %font,%bloc_list,"e2f; Federal Europe",%p_tlyear,3, _
  ";;0;@nan(100*(eu_ygm$-eu_ygx$)/eu_y$,0):" _
  )


'series LGVU_EUW_e2f = LGV_EUW_e2f
'LGV_EUW_e2f = 100*(LG_EUW_e2f-eu_lg_e2f)/V_EUW_e2f

series EU_LG_EUW_e1b = 0
series EU_LG_EUN_e1b = 0
series EU_LG_EUS_e1b = 0
series EU_LG_EUE_e1b = 0
series EU_LG_UK_e1b = 0

call SPGraph(%p_Page, %p_grp_code + "_govt_debt", _
  "government debt" _
    + "\n(% GDP)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;LGV_? | (LG_?-EU_LG_?)*100/V_?:" _
  )


' Interest Graph
'show irm_eus_e2f*lg_eus_e2f(-1)/v_eus_e2f (irm_eus_e2f*(lg_eus_e2f(-1)-eu_lg_eus_e2f(-1)) + (eu_lg_eus_e2f(-1)*2))/v_eus_e2f

call SPGraph(%p_Page, %p_grp_code + "_bond_rate_r", _
  "real bond rate" _
    + "\n(%)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;irm_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_bond_rate_n", _
  "nominal bond rate" _
    + "\n(%)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;im_?:" _
  )

series ILGADJ_EUW_e1b = 0
series ILGADJ_EUN_e1b = 0
series ILGADJ_EUS_e1b = 0
series ILGADJ_EUE_e1b = 0
series ILGADJ_UK_e1b = 0

call SPGraph(%p_Page, %p_grp_code + "_interest_payments", _
  "interest payments" _
    + "\n(% of GDP)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;100*(LG_?(-1)*irm_?*0.01)/V_? without RF | 100*((LG_?(-1)*irm_?*0.01)-iLGADJ_?)/V_? with RF:" _
  )




call SPGraph(%p_Page, %p_grp_code + "_govt_iago", _
  "govt fin transactions (other)" _
    + "\n(% GDP)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;IAGOV_?:" _
  )


' -- Financial balances

call SPGraph(%p_Page, %p_grp_code + "_private_net_lending", _
  "private sector net lending" _
    + "\n(% GDP)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;NLPV_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_three_bals", _
  "three balances" _
    + "\n(% GDP)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;NLPV_? private |NLGV_? government |CAV$_? foreign:" _
  )

' -- Other

call SPGraph(%p_Page, %p_grp_code + "_real_exchange_rate", _
  "real exchange rate" _
    + "\n(% GDP)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;rx_?:" _
  )

' Quickly generate RX series against USD
series rx$_eus_e1b = rx_eus_e1b/rx_us_e1b
series rx$_euw_e1b = rx_euw_e1b/rx_us_e1b
series rx$_eun_e1b = rx_eun_e1b/rx_us_e1b
series rx$_eue_e1b = rx_eue_e1b/rx_us_e1b
series rx$_uk_e1b = rx_uk_e1b/rx_us_e1b

series rx$_eus_e2f = rx_eus_e2f/rx_us_e2f
series rx$_euw_e2f = rx_euw_e2f/rx_us_e2f
series rx$_eun_e2f = rx_eun_e2f/rx_us_e2f
series rx$_eue_e2f = rx_eue_e2f/rx_us_e2f
series rx$_uk_e2f = rx_uk_e2f/rx_us_e2f


call SPGraph(%p_Page, %p_grp_code + "_real_dollar_xr", _
  "real exchange rate vs US" _
    + "\n(index)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;rx$_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_bond_rate", _
  "bond rate of interest" _
    + "\n(%)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;irm_?:" _
  )


' -- Employment and dependency


call SPGraph(%p_Page, %p_grp_code + "_empl_abs", _
  "employment" _
    + "\n(employment)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;NEW_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_empl_rate", _
  "employment rate" _
    + "\n(total employment as % of adult population)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;NER_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_empl_rate_wa", _
  "employment rate" _
    + "\n(working-age employment as % of working-age population)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";50,85;;NERW_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_empl_rate_mf", _
  "male and female employment rates" _
    + "\n(employment as % of working-age population)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";20,100;;NERMW_? Male Employment|NERFW_? Female Employment:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_productivity", _
  "Labour Productivity " + "\n(ouput per worker in ppp terms)", _
  %color, %font, %bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;VNE_? :" _
  )

' - Dependency Ratios

call SPGraph(%p_Page, %p_grp_code + "_dr_old_age", _
  "old-age dependency ratio"  _
    + "\n(persons aged 65 and over per working-age person)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";0.15,0.50;0;NDOR_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_dr_young", _
  "young dependency ratio" _
    + "\n(persons aged 0-14 as per working-age person", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";0.15,0.50;0;NDCR_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_dr_age", _
  "total age-based dependency ratio" _
    + "\n(persons aged 0-14 and 65 and over per working-age person)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";0.4,0.8;0;NDTR_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_dr_econ", _
  "Economic Dependency Ratio ", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";0.9,2.1;0;NDER1_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_dr_demog", _
  "Demographic Depency Ratio" _
    + "\n(ratio of old plus young to employed persons)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";0.6,2.0;0;NDDR1_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_dr_working_age", _
  "Working-age Dependency Ratio" _
    + "\n(ratio of inactive plus unemployed to employed persons)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";0.6,2.0;0;NDWR1_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_dr_econ2", _
  "Economic Dependency Ratio ", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";0.9,2.1;0;NDER2_?:" _
  )
call SPGraph(%p_Page, %p_grp_code + "_dr_demog2", _
  "Demographic Depency Ratio" _
    + "\n(ratio of old plus young to employed persons)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";0.6,2.0;0;NDDR2_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_dr_working_age2", _
  "Working-age Dependency Ratio" _
    + "\n(ratio of inactive plus unemployed to employed persons)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";0.6,2.0;0;NDWR2_?:" _
  )


' -- Population and migration

call SPGraph(%p_Page, %p_grp_code + "_population", _
  "population" _
    + "\n(millions of persons)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";-1,1;0;N_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_net_migration", _
  "net migration" _
    + "\n(millions of persons)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;NIM_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_net_migration", _
  "net migration rate" _
    + "\n(net migration as % of employed population)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";-1,2.5;0;NIME_?:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_population_wa", _
  "Working-Age Employed and Working-Age Population" _
    + "\n(millions of persons)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;NWP_? Working-Age Population:" _
  )

call SPGraph(%p_Page, %p_grp_code + "_population_dep", _
  "Dependent population " + "\n(millions of people)", _
  %color, %font, %bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;(NE_?*NDER2_?) :" _
  )

call SPGraph(%p_Page, %p_grp_code + "_productivity_dep", _
  "Labour Productivity and Labour Productivity / EDR" + "\n(ouput per employed/dependent in ppp terms)", _
  %color, %font, %bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;VNE_? Labour Productivity | NVDR2_? Output per dependent :" _
  )

call SPGraph(%p_Page, %p_grp_code + "_gov_exp_dep", _
  "Government spending per dependent person", _
  %color, %font, %bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;G_?/(NE_?*NDER2_?) :" _
  )

endsub
