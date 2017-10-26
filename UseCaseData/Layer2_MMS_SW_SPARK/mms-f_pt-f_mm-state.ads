private
package MMS.F_PT.F_MM.State is

   -----------------
   -- Input_State --
   -----------------

   Input_Navigation_Parameters : Navigation_Parameters_Type_Option with
     Part_Of => Input_State;

   Input_Navigation_Mode : Navigation_Mode_Type_Option with
     Part_Of => Input_State;

   Input_Navigation_Option : Navigation_Option_Type_Option with
     Part_Of => Input_State;

   Input_Go : Boolean with Part_Of => Input_State;

   Input_On_OFF_Push_Button : Boolean with Part_Of => Input_State;

   Input_Start_Push_Button : Boolean with Part_Of => Input_State;

   Input_Mode_Switch : Navigation_Mode_Type with Part_Of => Input_State;

   Input_Bay_Switch : Bay_Switch_Type with Part_Of => Input_State;

   Input_Payload_Mass : Payload_Mass_Type with Part_Of => Input_State;

   Input_USB_Key : USB_Key_Type_Option with
     Part_Of => Input_State;

   Input_Mission_Abort : Boolean with Part_Of => Input_State;

   Input_Estimated_Total_Mass : Estimated_Total_Mass_Type with
     Part_Of => Input_State;

   Input_Current_Range : Current_Range_Type with Part_Of => Input_State;

   Input_Current_Speed : Current_Speed_Type with Part_Of => Input_State;

   Input_Current_Altitude : Current_Altitude_Type with Part_Of => Input_State;

   Input_Current_Flight_Phase : Flight_Phase_Type with Part_Of => Input_State;

   Input_Energy_Level : Energy_Level_Type with Part_Of => Input_State;

   -------------------
   -- Private_State --
   -------------------

   Power_State : Power_State_Type with Part_Of => Private_State;

   On_State : On_State_Type with Part_Of => Private_State;

   Init_State : Init_State_Type with Part_Of => Private_State;

   Running_State : Running_State_Type with Part_Of => Private_State;

   Aborted_For_Energy_Reasons : Boolean with Part_Of => Private_State;

   --------------------------------
   -- Navigation_Parameter_State --
   --------------------------------

   Navigation_Mode : Navigation_Mode_Type with
     Part_Of => Navigation_Parameter_State;

   Operating_Mode_From_Parameters : Navigation_Option_Type with
     Part_Of => Navigation_Parameter_State;

   Operating_Mode : Navigation_Option_Type with
     Part_Of => Navigation_Parameter_State;

   Navigation_Parameters : Navigation_Parameters_Type with
     Part_Of => Navigation_Parameter_State;

   ---------------------------
   -- Operating_Point_State --
   ---------------------------

   Mission_Range  : Current_Range_Type with Part_Of => Operating_Point_State;

   Operating_Point : Operating_Point_Type with
     Part_Of => Operating_Point_State;

   ---------------------------
   -- Viability_Logic_State --
   ---------------------------

   Initial_Energy_Compatible_With_Mission : Boolean with
     Part_Of => Viability_Logic_State;

   In_Flight_Energy_Compatible_With_Mission : Boolean with
     Part_Of => Viability_Logic_State;

   -------------------------------
   -- Mission_Termination_State --
   -------------------------------

   Descent_Over : Boolean with Part_Of => Mission_Termination_State;

   ------------------
   -- Output_State --
   ------------------

   Output_Mission_Cancelled : Boolean with Part_Of => Output_State;

   Output_Mission_Complete : Boolean with Part_Of => Output_State;

   Output_Mission_Aborted : Boolean with Part_Of => Output_State;

   Output_Emergency_Landing : Boolean with Part_Of => Output_State;

   Output_Start_Take_Off : Boolean with Part_Of => Output_State;

   Output_Start_Landing : Boolean with Part_Of => Output_State;

   Output_Operating_Point : Operating_Point_Type with Part_Of => Output_State;

   Output_Operating_Mode : Navigation_Option_Type with Part_Of => Output_State;

   Output_Mission_Range : Current_Range_Type with Part_Of => Output_State;

end MMS.F_PT.F_MM.State;
