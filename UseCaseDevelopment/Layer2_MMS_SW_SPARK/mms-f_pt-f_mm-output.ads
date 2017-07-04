with Types; use Types;

package MMS.F_PT.F_MM.Output is

   -------------
   -- To F_CM --
   -------------
   
   function Mission_Cancelled return Boolean with Global => Output_State;
   
   function Mission_Complete return Boolean with Global => Output_State;
   
   function Mission_Aborted return Boolean with Global => Output_State;
   
   function Ready_For_Takeoff return Boolean with Global => Output_State;
   
   -------------
   -- To F_EL --
   --------------

   function Emergency_Landing return Boolean with Global => Output_State;
   
   -------------
   -- To F_FC --
   -------------
   
   function Start_Take_Off return Boolean with Global => Output_State;
   
   function Start_Landing return Boolean with Global => Output_State;

   function Operating_Point return Operating_Point_Type with Global => Output_State;
   
   function Operating_Mode return Navigation_Option_Type with Global => Output_State;
   
   function Mission_Range return Current_Range_Type with Global => Output_State;
   -- ??? which distance type
   
end MMS.F_PT.F_MM.Output;
