with Types; use Types;

package MMS.F_EL.Output is

   ------------
   -- To MMS --
   ------------
   
   function Propulsion_Torque return Torque_Type with Global => Output_State;
   
   function Braking_Torque return Torque_Type with Global => Output_State;
   
   -------------
   -- To F_PT --
   -------------
   
   function Mission_Abort return Boolean with Global => Output_State;

end MMS.F_EL.Output;
