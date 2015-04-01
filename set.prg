'PROGRAM: set.prg          Copyright (C) 2014 Alphametrics Co. Ltd.
'
' CAM Version 5.2 FESSUD variant
'
' settings
'
' this program fragment is included in all executable programs
' it initialises the system and provides common settings
'
' updated: FC 16/12/2014
'
'=======================================================

'--- set the current directory to this area and load libraries
%s = @runpath
cd {%s}
include "zlib"
include "zlibg"
include "zlibr"
include "zlibs"
include "zdef"
include "zrep"
include "zimpact"

'--- title for reports
%sysTitle = "CAM 5.2 with FESSUD blocs"

'--- data source (historical series)
!nSeries = 3007

'--- default simulation horizon
%predict = "2030"
'--- start year for simulation charts and reports
%repstart = "1980"
'--- columns in simulation tables
%yrsol = "2000 2008 2010 2011 2012 2015 2020 2030"

'--- historical data and alignment periods
%start = "1970"               ' first year of historical data
%base = "2005"                ' base year
%latest = "2012"              ' latest historical data
%align = "2014"               ' default alignment horizon
%end    = "2030"              ' end of workfile

'--- columns in history and alignment tables
%yrdat = "1980 1990 2000 2008 2009 2010 2011 2012"
%yrext = "1990 2000 2008 2009 2010 2011 2012 2014"

'--- graph coverage
'    B blocs, F first blocs, O other blocs,
'    G groups, W world
%repgeo = "FOG"
%subgraphs = "No"
'--- tables with data from graphs to be generated
'    on the tables page with same names as the graphs
%grtab = "No"

'--- bloc charts can be displayed in two groups
'    F first and O other (the rest)
'    this parameter selects the number of blocs in the
'    first group counting from the top of the list
!nFirstBlocs = 6

'--- blocs
%blocs = "" _
  + "de;Germany:" _
  + "fr;France:" _
  + "uk;UK:" _
  + "euc;Core eurozone:" _
  + "eup;Eurozone periphery:" _
  + "oeu;Other Europe:" _
  + "us;USA:" _
  + "cn;China:" _
  + "in;India:" _
  + "br;Brazil:" _
  + "id;Indonesia:" _
  + "za;South Africa:" _
  + "tr;Turkey:" _
  + "cis;CIS and other:" _
  + "can;Canada,Australia,New Zealand:" _
  + "oeh;Other East Asia High Income:" _
  + "nwa;North Africa and Other West Asia:" _
  + "acx;Central America and Caribbean:" _
  + "oam;Other South America:" _
  + "oea;Other East Asia:" _
  + "osa;Other South Asia:" _
  + "afl;Africa Low Income:"

'--- groups of blocs: change as required
'    the groups can overlap (a bloc can be in
'    more than one group) 
%groups = "" _
 + "BRICS; BRICS;" _
    + "br cis in cn za:" _
 + "EUR; Europe;" _
    + "de fr uk euc eup oeu:" _
 + "ROW;Rest of world;" _
    + "id tr us can oeh nwa acx oam oea osa afl:" _
 + "OHI;Other high income;" _
    + "us oeh can:" _
 + "OMI;Other middle income;" _
    + "tr nwa acx oam:" _
 + "OLI;Other low income;" _
    + "id oea osa afl:"

'=======================================================
