with Types; use Types;

package MMS.F_PT.F_EM.Output is

   -------------
   -- To F_CM --
   -------------
   
   function Primary_Source return Source_Type with Global => Output_State;
   
   function Secondary_Source return Source_Type with Global => Output_State;
   
   -------------
   -- To F_MM --
   -------------
   
   function Energy_Level return Energy_Level_Type with Global => Output_State;
   
end MMS.F_PT.F_EM.Output;
