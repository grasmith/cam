 'BATCH EXECUTION
'
'This program may be used to execute CAM programs sequentially without
'stopping.
'
'To select programs for execution, remove comments before call statements.
'
'NB the first two statements in each program file listed in the include
'section below must be commented out. If they are not commented out, EViews
'attempts to include the "set" file twice and stops with an error message.
'
'        e.g. in the file dat.prg, comment the lines
'                                  include "set"
'                                  call dat
'=========================================================================

'--- include configs and libraries
include "set"
'--- include MB library
include "mb"
'--- include program files
include "sol1a"
include "sol3a"
include "sol3b"

'--- execute programs
call sol1a
call sol3a
call sol3b

