package MMS.F_PT.F_MM.Output is

   -------------
   -- To F_CM --
   --------------
   
   function Mission_Cancelled return Boolean;
   
   function Mission_Complete return Boolean;
   
   function Mission_Aborted return Boolean;
   
   -------------
   -- To F_EL --
   --------------

   function Emergency_Landing return Boolean;
   
   -------------
   -- To F_FC --
   -------------
   
   function Start_Take_Off return Boolean;
   
   function Start_Landing return Boolean;

   function Operating_Point return Operating_Point_Type;
   
   function Operating_Mode return Navigation_Option_Type;
   
   function Mission_Range return Current_Range_Type; -- ??? which distance type
   
end MMS.F_PT.F_MM.Output;
