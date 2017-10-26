with MMS.F_PT.F_MM.State; use MMS.F_PT.F_MM.State;
package body MMS.F_PT.F_MM with
SPARK_Mode,
  Refined_State => (Input_State   =>
                      (Input_Navigation_Parameters,
                       Input_Navigation_Mode,
                       Input_Navigation_Option,
                       Input_Go,
                       Input_On_OFF_Push_Button,
                       Input_Start_Push_Button,
                       Input_Mode_Switch,
                       Input_Bay_Switch,
                       Input_Payload_Mass,
                       Input_USB_Key,
                       Input_Mission_Abort,
                       Input_Estimated_Total_Mass,
                       Input_Current_Range,
                       Input_Current_Speed,
                       Input_Current_Altitude,
                       Input_Current_Flight_Phase,
                       Input_Energy_Level),
                    Output_State  =>
                      (Output_Mission_Cancelled,
                       Output_Mission_Complete,
                       Output_Mission_Aborted,
                       Output_Emergency_Landing,
                       Output_Start_Take_Off,
                       Output_Start_Landing,
                       Output_Operating_Point,
                       Output_Operating_Mode,
                       Output_Mission_Range),
                    Private_State =>
                      (Power_State,
                       On_State,
                       Init_State,
                       Running_State,
                       Aborted_For_Energy_Reasons),
                    Navigation_Parameter_State =>
                      (Navigation_Mode,
                       Operating_Mode_From_Parameters,
                       Operating_Mode,
                       Navigation_Parameters),
                    Operating_Point_State =>
                      (Mission_Range,
                       Operating_Point),
                    Viability_Logic_State =>
                      (Initial_Energy_Compatible_With_Mission,
                       In_Flight_Energy_Compatible_With_Mission),
                    Mission_Termination_State =>
                      (Descent_Over))
is
end MMS.F_PT.F_MM;
