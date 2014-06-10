'BATCH EXECUTION
'
'This program may be used to execute CAM programs sequentially
' without stopping.
'
'The following statements in each program file listed here must be
'commented out:
'
' include "set"
' call <xxx>             where <xxx> is the program name

' If these statements are not commented out, EViews will stop
' with an error message.
'
'===================================================================

'--- include configs and libraries
include "set"
'--- include program files
include "dat"
include "est"
include "sola"
include "sol0p"
include "sol0"
include "sol1"
include "sol2"

'--- execute programs
call dat
call est
call sola
call sol0p
call sol0
call sol1
call sol2
