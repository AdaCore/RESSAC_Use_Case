with MMS.F_PT.F_FC.Output;
with MMS.F_PT.F_CM.Output;

with Types; use Types;

package MMS.F_PT.F_EM.Input is

   ---------------
   -- From F_FC --
   ---------------
   
   function Propulsion_Torque return Torque_Type
     renames MMS.F_PT.F_FC.Output.Propulsion_Torque;
   
   function Braking_Torque return Torque_Type
     renames MMS.F_PT.F_FC.Output.Propulsion_Torque;
   
   ---------------
   -- From F_CM --
   ---------------
   
   function P_Dot return Speed_Type
     renames MMS.F_PT.F_CM.Output.P_Dot;

end MMS.F_PT.F_EM.Input;
