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
'        e.g. in the file dat.prg comment the lines
'                                   include "set"
'                                   call dat
'=========================================================================

'--- include configs and libraries
include "set"
'--- multi-bloc budget (special)
include "mb"
'--- program files
include "dat"
include "est"
include "sola"
include "sol0p"
include "sol0pj"
include "sol0"
include "sol0f"
include "solE1"
include "solE1a"
include "solE2"
include "solE2a"
include "solE3"
include "solE3a"
'include "solE4"
'include "solE4a"
include "sole1b"
'include "soled2-06-05"
include "sole2f"

'--- execute programs
'call dat
'call est
'call sola
'call sol0p
'call sol0pj
'call sol0
'call solE1
'call solE1a
'call solE2
'call solE2a
'call solE3
'call solE3a
'call solE4
'call solE4a
'call sol0f
'call sole1b
'call sole2d
call sole2f

