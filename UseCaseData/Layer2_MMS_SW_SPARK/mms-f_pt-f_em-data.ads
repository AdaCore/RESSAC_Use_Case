with Types; use Types;

private
package MMS.F_PT.F_EM.Data with SPARK_Mode is

   ---------------
   -- Constants --
   ---------------
   
   --  From 6.8.4
   
   Primary_Initial_Capacity   : Energy_Level_Type 
     with Part_Of => Private_State;
   Secondary_Initial_Capacity : Energy_Level_Type
     with Part_Of => Private_State; 

end MMS.F_PT.F_EM.Data;
