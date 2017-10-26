with MMS.F_PT.Output;

with Types; use Types;

package MMS.Output is

   -------------------------------------
   -- Ground-based Mission Monitoring --
   -------------------------------------
   
   function CP_Switches return CP_Switches_Type 
     renames MMS.F_PT.Output.CP_Switches;
   
   function CP_Displays return CP_Displays_Type 
     renames MMS.F_PT.Output.CP_Displays;
   
   function Propulsion_Energy return Propulsion_Energy_Type 
     renames MMS.F_PT.Output.Propulsion_Energy;
   
   ----------------------------
   -- Control Panel Displays --
   ----------------------------
   
   function Mission_Cancelled return Boolean 
     renames MMS.F_PT.Output.Mission_Cancelled;
   
   function Mission_Complete return Boolean 
     renames MMS.F_PT.Output.Mission_Complete;
   
   function Mission_Aborted return Boolean 
     renames MMS.F_PT.Output.Mission_Aborted;
   
   function Primary_Source return Source_Type 
     renames MMS.F_PT.Output.Primary_Source;
   
   function Secondary_Source return Source_Type 
     renames MMS.F_PT.Output.Secondary_Source;
   
   -------------------------
   -- Physical Parameters --
   -------------------------
   
   function Propulsion_Torque return Torque_Type;
   
   function Braking_Torque return Torque_Type;
   
end MMS.Output;
