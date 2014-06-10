'PROGRAM: ztab.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6
'
' analysis table definitions
'
' updated: FC 26/10/2011
'
'================================================

subroutine pTabDef(table t_Def, scalar nDef)
'==================================================
'load definitions of standard analysis tables
'
' Call: t_TDef  table of table definitions
'       nDef    no of rows 
'
' Ret:  t_TDef  updated table definitions
'       nDef    updated no of rows
'
' Note: the layout of a table definition is as follows
'         header row
'         1 or many section definitions
'         1 or many row definitions
'
'       header row syntax
'         T;tabname;title
'
'       section definition syntax
'         S;mode;transform;units
'           where mode is Annual or Period average
'           and transform is GR, Change, Value
'            or /expr (ratio) or %expr (per cent)
'
'       row definition syntax
'         R;expr;title;format;option
'         where expr may include placeholders
'          ? area + alias
'          ?? area only
'          ~ alias only
'         format is
'          ft.k to display k decimal places
'          optionally prefixed by U to draw a line
'          underneath the row of data 
'         option
'          NGR  exclude from growth rate sections
'
'---------------------------------------------------------------

call LoadTable(t_TDef, nTDef, _
  "T;expgdp;EXPENDITURE AND GDP GROWTH:" _
  + "S;P;GR;{growth rates, % p.a.):" _
  + "S;A;Value;{$ billion, 2005 pp):" _
  + "S;P;Change;{changes, $ billion, 2005 pp):" _
  + "R;C_?/1000;Consumers expenditure;ft.1:" _
  + "R;G_?/1000;Government expenditure on g & s;ft.1:" _
  + "R;IP_?/1000;Non-government fixed investment;ft.1:" _
  + "R;IV_?/1000;Inventory changes;Uft.1;NGR:" _
  + "R;H_?/1000;Total domestic demand;ft.1:" _
  + "R;(X0_?/pp0_??)/1000;Exports of goods and services;Uft.1:" _
  + "R;(H_?+X0_?/pp0_??)/1000;Total final demand;ft.1:" _
  + "R;(M0_?/pp0_??)/1000;Imports of goods and services;Uft.1:" _
  + "R;V_?/1000;GDP;ft.1:" _
  + "R;(VV_?-V_?)/1000;Terms of trade impact;ft.1;NGR:" _
  + "R;(BIT$_?/rx_?)/1000;External income and transfers;Uft.1;NGR:" _
  + "R;Y_?/1000;National income;ft.1:" _
  + "R;N_?;Population (millions);ft.1:" _
  + "R;YN_?;National income per capita ($ ppp);ft.0:" _
  + "R;C_?/N_?;Consumers exp per capita ($ ppp);ft.0:" _
  + "R;M$_?/N_?;Imports per capita ($);ft.0" _
)
  
call LoadTable(t_TDef, nTDef, _
  + "T;exp;EXPENDITURE PATTERN:" _
  + "S;A;%Y_?;{per cent of national income):" _
  + "R;C_?;Consumers expenditure;ft.1:" _
  + "R;G_?;Government expenditure on g & s;ft.1:" _
  + "R;IP_?;Non-government fixed investment;ft.1:" _
  + "R;IV_?;Inventory changes;Uft.1;NGR:" _
  + "R;H_?;Total domestic demand;ft.1:" _
  + "R;CA$_?/rx_?;Current account;Uft.1;NGR:" _
  + "R;Y_?;National income;ft.1:" _
)

call LoadTable(t_TDef, nTDef, _
  "T;exim;EXPORT AND IMPORTS:" _
  + "S;A;Value;($ billion, 2005 pp):" _
  + "S;A;/(N_?/1000);($ per capita):" _
  + "S;A;%(VV_?/1000);{per $100 of ppp GDP):" _
  + "R;XA$_?/1000;Primary commodity exports;ft.1:" _
  + "R;MA$_?/1000;Primary commodity imports;ft.1:" _
  + "R;BA$_?/1000;Primary commodity balance;ft.1:" _
  + "R;XE$_?/1000;Energy exports;ft.1:" _
  + "R;ME$_?/1000;Energy imports;ft.1:" _
  + "R;BE$_?/1000;Energy balance;ft.1:" _
  + "R;XM$_?/1000;Manufactures exports;ft.1:" _
  + "R;MM$_?/1000;Manufactures imports;ft.1:" _
  + "R;BM$_?/1000;Manufactures balance;ft.1:" _
  + "R;XS$_?/1000;Service exports;ft.1:" _
  + "R;MS$_?/1000;Service imports;ft.1:" _
  + "R;BS$_?/1000;Service balance;Uft.1:" _
  + "R;X$_?/1000;Goods and services exports;ft.1:" _
  + "R;M$_?/1000;Goods and services imports;ft.1:" _
  + "R;TB$_?/1000;Goods and services balance;ft.1:" _
  + "R;XIT$_?/1000;Income and transfer receipts;ft.1:" _
  + "R;MIT$_?/1000;Income and transfer payments;ft.1:" _
  + "R;BIT$_?/1000;Income and transfer balance;ft.1:" _
  + "R;CA$_?/1000;Current account balance;ft.1:" _
)

call LoadTable(t_TDef, nTDef, _
  "T;bop;BALANCE OF PAYMENTS:" _
  + "S;A;Value;($ billion, 2005 pp):" _
  + "S;A;%(VV_?/1000);(% of GDP):" _
  + "R;BA$_?/1000;Primary commodities;ft.1:" _
  + "R;BE$_?/1000;Energy;ft.1:" _
  + "R;BM$_?/1000;Manufactures;ft.1:" _
  + "R;BS$_?/1000;Services;Uft.1:" _
  + "R;(BA$_?+BE$_?+BM$_?+BS$_?)/1000;Total goods and services;ft.1:" _
  + "R;BIT$_?/1000;Income and transfers;Uft.1:" _
  + "R;(BA$_?+BE$_?+BM$_?+BS$_?+BIT$_?)/1000;Current account;Uft.1:" _
  + "R;(AXO$_?-LX$_?)/1000;Net external assets (excl reserves);ft.1:" _
  + "R;R$_?/1000;Reserves;ft.1:" _
)

call LoadTable(t_TDef, nTDef, _
  "T;lg;GOVERNMENT BUDGET AND DEBT:" _
  + "S;A;Value;($ billion, 2005 pp):" _
  + "S;A;%(VV_?/1000);(% of GDP):" _
  + "S;P;GR;(growth rates, % p.a.):" _
  + "R;YG_?/1000;Disposable income (net);ft.1:" _
  + "R;G_?/1000;Expenditure on goods and services;ft.1:" _
  + "R;(YG_?-G_?)/1000;" _
    + "Financial surplus (+) or deficit (-);Uft.1;NGR:" _
  + "R;IAG_?/1000;Net acquisition of financial assets;ft.1;NGR:" _
  + "R;ILG_?/1000;" _
    + "Net incurrence of financial liabilities;Uft.1;NGR:" _
  + "R;LG_?/1000;Outstanding debt;ft.1:" _
  + "R;VV_?/1000;GDP;ft.1:" _
) 

call LoadTable(t_TDef, nTDef, _
  "T;pixr;INFLATION AND THE EXCHANGE RATE:" _
  + "S;A;Value;(index 2005 = 100):" _
  + "S;P;GR;(average change, % p.a.):" _
  + "R;pvd_?;1. Domestic costs (GDP deflator);f.0:" _
  + "R;phd_?;2. Domestic prices (exp deflator);Uf.0:" _
  + "R;rxd_?;3. Exchange rate ($ per ccy unit);f.0:" _
  + "R;phd_?*rxd_?/100;4. Domestic prices in USD;f.0:" _
  + "R;100*ucx$_?;5. Unit cost of exports in USD;f.0:" _
  + "R;100*ph_w~;6. World prices in USD;Uf.0:" _
  + "R;100*rx_?/pp0_??;7. Real exchange rate;f.0:" _
  + "R;100*ucx$_?/ph_w~;8. Relative unit cost of exports;f.0:" _
)

call LoadTable(t_TDef, nTDef, _
  "T;eped;ENERGY SUPPLY AND USE:" _
  + "S;A;Value;(kg of oil equivalent):" _
  + "R;1000*EP_?/N_?;Energy supply per person;ft.0:" _
  + "R;1000*ED_?/N_?;Energy use per person;Uft.0:" _
  + "R;1000*(EX_?-EM_?)/N_?;Energy balance per person;Uft.0:" _
  + "R;1000*ED_?/V_?;Energy use in kg per $ of ppp GDP;ft.2:" _
)

endsub
