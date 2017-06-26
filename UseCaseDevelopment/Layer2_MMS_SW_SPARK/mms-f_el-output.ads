with Types; use Types;

package MMS.F_EL.Output is

   ------------
   -- To MMS --
   ------------
   
   function Propulsion_Torque return Torque_Type;
   
   function Braking_Torque return Torque_Type;
   
   -------------
   -- To F_PT --
   -------------
   
   function Mission_Abort return Boolean;

end MMS.F_EL.Output;
