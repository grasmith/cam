'LIBRARY: zlibj.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM Version 5.2
'
' additional library routines add by JM
'
' Note: routines whose names begin with z are internal. Names
' beginning with lib_ are reserved and should not be used
' elsewhere.
'
'
'---------------------------------------------------------------
' First(%t, %a, %r)
'---------------------------------------------------------------

include "zlibp"

subroutine local First(string %t, string %a, string %r)
'=================================================='
' Return the first item from a list without truncating the list
'
' Call: %t  source string
'       %a  separator character for list items
'
' Ret:  %t  string remainder
'       %r  token
'
'---------------------------------------------------------------
!i = @instr(%t, %a)
if !i > 0 then
  %r = @left(%t, !i-1)
else
  %r = %t
endif
%r = @trim(%r)
endsub

subroutine mk_extra_graphs(string %p_scenario_list)
'==================================================================
' Call: %p_scenario_list    list of scenario codes and names, e.g.
' "e1; austerity: e2; federalism"

call pLog("Generating additional presentation graphs")

%p_page_name="agraphs"

pagecreate(page={%p_page_name}) a %start %end
pageselect data

call mk_single_graphs(%p_scenario_list, %p_page_name)
endsub

subroutine mk_single_graphs(string %p_tlScenario, string %p_Page)
'==================================================================
' -- Generate sequence of graphs, each on a single pane
' Call: %p_scenario_list    list of scenario codes and names
'       %p_Page             name of workfile page to save graphs onto

%p_tlyear = "1980 2030 2015"

%font = "28"
%color = %blyellow


' -- Europe
%p_grp_code = "EU"
%bloc_list = "EUC FR EUP ENC UK ENE"

'-- GDP and components

call SPGraph(%p_Page, %p_grp_code + "_gdp_growth", _
  "GDP growth" _
    + "\n(%)", _
  %color, %font,%bloc_list,%p_tlScenario,%p_tlyear,3, _
  ";;0;DV0_?:" _
  )
endsub
