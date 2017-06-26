with MMS.F_PT.F_CM.Output;
with MMS.F_PT.F_MM.Output;
  
with Types; use Types;

package MMS.F_PT.Output is
   
   -----------
   --To MMS --
   -----------

   function CP_Switches return CP_Switches_Type 
     renames MMS.F_PT.F_CM.Output.CP_Switches;
   
   function CP_Displays return CP_Displays_Type
     renames MMS.F_PT.F_CM.Output.CP_Displays;
   
   function Propulsion_Energy return Propulsion_Energy_Type
     renames MMS.F_PT.F_CM.Output.Propulsion_Energy;
   
   function Mission_Cancelled return Boolean
     renames MMS.F_PT.F_CM.Output.Mission_Cancelled;
   
   function Mission_Complete return Boolean
     renames MMS.F_PT.F_CM.Output.Mission_Complete;
   
   function Mission_Aborted return Boolean
     renames MMS.F_PT.F_CM.Output.Mission_Aborted;
   
   function Primary_Source return Source_Type
     renames MMS.F_PT.F_CM.Output.Primary_Source;
   
   function Secondary_Source return Source_Type
     renames MMS.F_PT.F_CM.Output.Secondary_Source;
   
   function Propulsion_Torque return Torque_Type
     renames MMS.F_PT.F_CM.Output.Propulsion_Torque;
   
   function Braking_Torque return Torque_Type
     renames MMS.F_PT.F_CM.Output.Braking_Torque;

   --------------
   -- To F_EL --
   --------------
   
   function Emergency_Landing return Boolean
     renames MMS.F_PT.F_MM.Output.Emergency_Landing;
   
end MMS.F_PT.Output;
