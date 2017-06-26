with Types; use Types;

package MMS.F_PT.F_EM.Output is

   -------------
   -- To F_CM --
   -------------
   
   function Primary_Source return Source_Type;
   
   function Secondary_Source return Source_Type;
   
   -------------
   -- To F_MM --
   -------------
   
   function Energy_Level return Energy_Level_Type;
   
end MMS.F_PT.F_EM.Output;
