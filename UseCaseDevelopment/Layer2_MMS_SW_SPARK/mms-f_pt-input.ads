with MMS.Input;
with MMS.F_EL.Output;

with Types; use Types;

package MMS.F_PT.Input is

   --------------
   -- From MMS --
   --------------

   function Navigation_Parameters return Navigation_Parameters_Type 
     renames MMS.Input.Navigation_Parameters;

   function Navigation_Mode return Navigation_Mode_Type
     renames MMS.Input.Navigation_Mode;
   
   function Navigation_Option return Navigation_Option_Type
     renames MMS.Input.Navigation_Option;

   function Go return Boolean
     renames MMS.Input.Go;   

   function Emergency_Landing return Boolean
     renames MMS.Input.Emergency_Landing;

   function On_OFF_Push_Button return Boolean
     renames MMS.Input.On_OFF_Push_Button;

   function Start_Push_Button return Boolean
     renames MMS.Input.Start_Push_Button;

   function Mode_Switch return Navigation_Mode_Type
     renames MMS.Input.Mode_Switch;

   function Bay_Switch return Bay_Switch_Type
     renames MMS.Input.Bay_Switch; 

   function Payload_Mass return Payload_Mass_Type
     renames MMS.Input.Payload_Mass;
   
   function USB_Key return Navigation_Parameters_Type
     renames MMS.Input.USB_Key;
   
   function P return Distance_Type
     renames MMS.Input.P;
   
   function P_Dot return Speed_Type
     renames MMS.Input.P_Dot;
   
   function Q return Angle_Type
     renames MMS.Input.Q;
   
   ---------------
   -- From F_EL --
   ---------------
   
   function Mission_Abort return Boolean
     renames MMS.F_EL.Output.Mission_Abort;
   
end MMS.F_PT.Input;
