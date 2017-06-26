with Types; use Types;

package MMS.F_PT.F_MM.Output with
  Abstract_State => (To_F_CM, To_F_EL, To_F_FC)
is

   -------------
   -- To F_CM --
   --------------
   
   function Mission_Cancelled return Boolean with Global => To_F_CM;
   
   function Mission_Complete return Boolean with Global => To_F_CM;
   
   function Mission_Aborted return Boolean with Global => To_F_CM;
   
   -------------
   -- To F_EL --
   --------------

   function Emergency_Landing return Boolean with Global => To_F_EL;
   
   -------------
   -- To F_FC --
   -------------
   
   function Start_Take_Off return Boolean with Global => To_F_FC;
   
   function Start_Landing return Boolean with Global => To_F_FC;

   function Operating_Point return Operating_Point_Type with Global => To_F_FC;
   
   function Operating_Mode return Navigation_Option_Type with Global => To_F_FC;
   
   function Mission_Range return Current_Range_Type with Global => To_F_FC;
   -- ??? which distance type
   
end MMS.F_PT.F_MM.Output;
