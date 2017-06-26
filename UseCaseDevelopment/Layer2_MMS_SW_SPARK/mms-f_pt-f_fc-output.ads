with Types; use Types;

package MMS.F_PT.F_FC.Output is

   ----------------------
   -- To F_CM and F_EM --
   ----------------------

   function Propulsion_Torque return Torque_Type;
   
   function Braking_Torque return Torque_Type;
   
   -------------
   -- To F_MM --
   -------------
   
   function Mission_Abort return Boolean; -- ??? not listed in F_FC outputs
   
   function Estimated_Total_Mass return Estimated_Total_Mass_Type;
   
   function Current_Range return Current_Range_Type;
   
   function Current_Speed return Current_Speed_Type;
   
   function Current_Altitude return Current_Altitude_Type;
   
end MMS.F_PT.F_FC.Output;
