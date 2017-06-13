with MMS.F_PT.F_CM.Output;
with MMS.F_PT.F_MM.Output;

package MMS.F_PT.F_FC.Input is

   ---------------
   -- From F_CM --
   ---------------

   function P return Distance_Type
     renames MMS.F_PT.F_CM.Output.P;
   
   function P_Dot return Speed_Type
     renames MMS.F_PT.F_CM.Output.P_Dot;
   
   function Q return Altitude_Type
     renames MMS.F_PT.F_CM.Output.Q;
      
   ---------------
   -- From F_MM --
   ---------------
   
   function Start_Take_Off return Boolean
     renames MMS.F_PT.F_MM.Output.Start_Take_Off;
   
   function Start_Landing return Boolean
     renames MMS.F_PT.F_MM.Output.Start_Landing;

   function Operating_Point return Operating_Point_Type
     renames MMS.F_PT.F_MM.Output.Operating_Point;
   
   function Operating_Mode return Navigation_Option_Type
     renames MMS.F_PT.F_MM.Output.Operating_Mode;
   
   function Mission_Range return Current_Range_Type
     renames MMS.F_PT.F_MM.Output.Mission_Range; 

end MMS.F_PT.F_FC.Input;
