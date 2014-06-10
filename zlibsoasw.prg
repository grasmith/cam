

'==================================================================
subroutine mk_workfile
call zCreateWk("feps_data")
endsub

'==================================================================
subroutine upd_workfile

output(s) sp_log
show sp_log

pagecreate(page="dependency") a %start %end
'pagecreate(page="govtservice") a %start %end

pageselect data
call mk_graphs
pagedelete data

' save with a different name
wfsave feps-a
endsub


' -- Two subroutines borrowed from elsewhere

'==================================================================
subroutine zCreateWk(string %p_gWk)
open solE3a
t_Settings(7,2) = %p_gWk
pagedelete tables
pagedelete graphs
'pagecreate(page={%p_gWk}) a %start %end
wfsave {%p_gWk}
pageselect data
output(s) sp_log
show sp_log
call pLog(%p_gWk + " REPORTS v2408")
call pLog("Loading data from scenario workfiles")
'call LoadScenario("E1")
'call LoadScenario("E2a")
'call LoadScenario("E2")
'call LoadScenario("E4")
call LoadScenario("E4a")
'call LoadScenario("E3a")
'call LoadScenario("E3")
wfsave {%p_gWk}
endsub



'---------------------------------------------------------------
' LoadScenario(%Alias)
' SPGraph(%Page, %Name, %Title, %Color, %Font, %tlBloc, %tlAlias, 
'   %tlYear, nCol, %tlContent)
'
'   NB: content = title;scale;options;series list
'---------------------------------------------------------------

subroutine LoadScenario(string %p_Alias)
'==================================================
'load series from another scenario workfile
'
' Call: %p_Alias     alias to load
'
' Ret:
'
'---------------------------------------------------------------
%lib_wk0 = t_Settings(7,2)
%lib_wk1 = "SOL" + %p_Alias 
call pLog("loading series for scenario " + %p_Alias)
open {%lib_wk1}
wfselect {%lib_wk0}
smpl %start %end
copy {%lib_wk1}::data\*_{%p_Alias}
close {%lib_wk1}
endsub





' --- 
' --- mk_multi_graphs
' ---

subroutine mk_multi_graphs(string %p_tlScenario)

%font = "28"
%color = %blyellow

' -- Generate three-part graphs

%scenarios =  "continued austerity, coordinated European action"


'"struggling on plus reduced government, " + _
' "EU breakup plus US China reaction, " + _
' "multi-speed Europe plus regionalisation, " + _
' "toward Federal Europe plus multipolar collaboration"

' "EU breakup plus reduced government, " + _
' "towards Federal Europe plus regionalisation, " + _

' -- Europe graphs
 

for %eu_bloc %bloc_label "EUS" "South Europe" "EUN" "North Europe" "UK" "UK" "EUW" "West Europe" "EUE" "East Europe"
table temp

call zSubGraph("__aa",%eu_bloc, %p_tlScenario, _
  "Economic Dependency Ratio;1.0,2.0;0;NDER1_?", _
  "1990", "2030", "2012 2030", %font, "temp", 0)

call zSubGraph("__ab",%eu_bloc, %p_tlScenario, _
  "Demographic Dependency Ratio;0.2,1.0;0;NDDR1_?", _
  "1990", "2030", "2012 2030", %font, "temp",  0)

call zSubGraph("__ac",%eu_bloc, %p_tlScenario, _
  "Working-Age Dependency Ratio;0.2,1.0;0;NDWR1_?", _
  "1990", "2030", "2012 2030", %font, "temp",  0)

call zMergeGraph("dependency", "nder_" + %eu_bloc, "Economic Dependency Ratio and its components\n" + %bloc_label,  "2012 2030", 3, %color, %font, "__aa __ab __ac", %scenarios, 6, 6, "temp") 

next

for %eu_bloc %bloc_label "EUS" "South Europe" "EUN" "North Europe" "UK" "UK" "EUW" "West Europe" "EUE" "East Europe"
table temp

call zSubGraph("__aa",%eu_bloc, %p_tlScenario, _
  "Economic Dependency Ratio;1.0,2.0;0;NDER2_?", _
  "1990", "2030", "2012 2030", %font, "temp", 0)

call zSubGraph("__ab",%eu_bloc, %p_tlScenario, _
  "Demographic Dependency Ratio;0.2,1.0;0;NDDR2_?", _
  "1990", "2030", "2012 2030", %font, "temp",  0)

call zSubGraph("__ac",%eu_bloc, %p_tlScenario, _
  "Working-Age Dependency Ratio;0.2,1.0;0;NDWR2_?", _
  "1990", "2030", "2012 2030", %font, "temp",  0)

call zMergeGraph("dependency", "nder_" + %eu_bloc, "Economic Dependency Ratio and its components\n" + %bloc_label,  "2012 2030", 3, %color, %font, "__aa __ab __ac", %scenarios, 6, 6, "temp") 

next

for %eu_bloc %bloc_label "EUS" "South Europe" "EUN" "North Europe" "UK" "UK" "EUW" "West Europe" "EUE" "East Europe"
table temp

call zSubGraph("__aa",%eu_bloc, %p_tlScenario, _
  "Labour Productivity;;0;VNE_?", _
  "1990", "2030", "2012 2030", %font, "temp", 0)

call zSubGraph("__ab",%eu_bloc, %p_tlScenario, _
  "EDR/Productivity (Output per Dependent);;0;NVDR2_?", _
  "1990", "2030", "2012 2030", %font, "temp",  0)

call zMergeGraph("dependency", "ndvr_" + %eu_bloc, "Labour Productivity and Dependency \n" + %bloc_label,  "2012 2030", 3, %color, %font, "__aa __ab", %scenarios, 6, 6, "temp") 

next

endsub

