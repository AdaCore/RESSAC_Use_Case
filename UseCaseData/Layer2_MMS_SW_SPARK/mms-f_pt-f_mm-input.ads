with MMS.F_PT.F_CM.Output;
with MMS.F_PT.F_FC.Output;
with MMS.F_PT.F_EM.Output;
with MMS.F_PT.Input;

with Types; use Types;

package MMS.F_PT.F_MM.Input is
   
   ---------------
   -- From F_CM --
   ---------------

   function Navigation_Parameters return Navigation_Parameters_Type_Option
     renames MMS.F_PT.F_CM.Output.Navigation_Parameters;

   function Navigation_Mode return Navigation_Mode_Type_Option
     renames MMS.F_PT.F_CM.Output.Navigation_Mode;
   
   function Navigation_Option return Navigation_Option_Type_Option
     renames MMS.F_PT.F_CM.Output.Navigation_Option;

   function Go return Boolean
     renames MMS.F_PT.F_CM.Output.Go;   

   function On_OFF_Push_Button return Boolean
     renames MMS.F_PT.F_CM.Output.On_OFF_Push_Button;

   function Start_Push_Button return Boolean
     renames MMS.F_PT.F_CM.Output.Start_Push_Button;

   function Mode_Switch return Navigation_Mode_Type
     renames MMS.F_PT.F_CM.Output.Mode_Switch;

   function Bay_Switch return Bay_Switch_Type
     renames MMS.F_PT.F_CM.Output.Bay_Switch;
   
   function USB_Key return USB_Key_Type_Option
     renames MMS.F_PT.F_CM.Output.USB_Key;
   
   function Rotactor_1 return Rotactor_Type
     renames MMS.F_PT.F_CM.Output.Rotactor_1;
   
   function Rotactor_2 return Rotactor_Type
     renames MMS.F_PT.F_CM.Output.Rotactor_2;
   
   -----------------------
   -- From F_FC or F_EL --
   -----------------------
   
   function Mission_Abort return Boolean is
      (MMS.F_PT.F_FC.Output.Mission_Abort or else MMS.F_PT.Input.Mission_Abort);
   
   ---------------
   -- From F_FC --
   ---------------

   function Current_Range return Current_Range_Type
     renames MMS.F_PT.F_FC.Output.Current_Range;
   
   function Current_Speed return Current_Speed_Type
     renames MMS.F_PT.F_FC.Output.Current_Speed;
   
   function Current_Altitude return Current_Altitude_Type
     renames MMS.F_PT.F_FC.Output.Current_Altitude;
   
   function Current_Flight_Phase return Flight_Phase_Type
     renames MMS.F_PT.F_FC.Output.Current_Flight_Phase;
   
   ---------------
   -- From F_EM --
   ---------------
   
   function Energy_Level return Energy_Level_Type
     renames MMS.F_PT.F_EM.Output.Energy_Level;
   
end MMS.F_PT.F_MM.Input;
