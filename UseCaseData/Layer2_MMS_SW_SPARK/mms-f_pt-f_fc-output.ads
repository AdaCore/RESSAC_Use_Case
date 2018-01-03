with Types; use Types;

package MMS.F_PT.F_FC.Output is

   ---------------------
   -- To F_CM or F_EM --
   ---------------------

   function Propulsion_Torque return Torque_Type with Global => Output_State;
   
   function Braking_Torque return Torque_Type with Global => Output_State;
   
   -------------
   -- To F_MM --
   -------------
   
   function Mission_Abort return Boolean with Global => Output_State; -- ??? not listed in F_FC outputs
   
   function Current_Range return Current_Range_Type with Global => Output_State;
   
   function Current_Speed return Current_Speed_Type with Global => Output_State;
   
   function Current_Altitude return Current_Altitude_Type with Global => Output_State;
   
   function Current_Flight_Phase return Flight_Phase_Type with Global => Output_State;
   
end MMS.F_PT.F_FC.Output;
