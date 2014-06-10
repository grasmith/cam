'PROGRAM: set.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6 AUGUR variant
'
' settings
'
' this program fragment is included in all executable programs
' it initialises the system and provides common settings
'
' updated: FC 15/04/2012
'
'=======================================================

'--- set the current directory to this area
%s = @runpath
cd {%s}

'--- title for reports
%sysTitle = "CAM 4.6 with AUGUR blocs"

'--- load libraries
include "zlib"
include "zlibg"
include "zlibr"
include "zlibs"
include "zdef"
include "zrep"
include "zimpact"

'--- simulation horizon
%predict = "2030"
'--- start year for simulation charts and reports
%repstart = "1980"
'--- columns in simulation tables
%yrsol = "2000 2008 2010 2011 2012 2015 2020 2030"

'--- data source (historical series)
!nSeries = 1859

'--- bloc lists
%blocs = "eun;North Europe:" _
       + "euw;West Europe:" _
       + "uk;UK:" _
       + "eus;South Europe:" _
       + "eue;East Europe:" _
       + "us;USA:" _
       + "ja;Japan:" _
       + "od;Other Developed:" _
       + "eah;East Asia High Income:" _
       + "ci;CIS:" _
       + "wa;West Asia:" _
       + "ams;South America:" _
       + "acx;Central America:" _
       + "cn;China:" _
       + "eao;Other East Asia:" _
       + "in;India:" _
       + "aso;Other South Asia:" _
       + "afn;North Africa:" _
       + "afs;Other Africa"

'--- bloc aggregates
%groups = "eu;Europe;eun euw uk eus eue:" _
        + "am;America;us od acx ams:" _
        + "af;Africa;afn afs:" _
        + "oa;Other Asia;ci wa in aso:" _
        + "ea;East Asia;ja cn eah eao:" _
        + "cwa;European neighbours;ci wa afn:" _
        + "nam;North America;us od acx:"

'--- history and solution periods
%start = "1970"               ' first year of historical data
%base = "2005"                ' base year
%latest = "2010"              ' latest historical data
%align = "2012"               ' alignment horizon
%end = "2030"                 ' end of workfile (max series length)

'--- columns in history and alignment tables
%yrdat = "1971 1980 1990 1995 2000 2005 2008 2009"
%yrext = "1990 2000 2007 2008 2009 2010 2011 2012"

'=======================================================
