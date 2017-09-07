with Types; use Types;

package MMS.F_PT.F_EM with 
  SPARK_Mode,
  Abstract_State => 
  (Private_State,
   Output_State) 
is
   pragma Elaborate_Body (MMS.F_PT.F_EM);
end MMS.F_PT.F_EM;
