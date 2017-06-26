with MMS.F_PT.Input;
with MMS.F_PT.F_MM.Output;
with MMS.F_PT.F_FC.Output;
with MMS.F_PT.F_EM.Output;

with Types; use Types;

package MMS.F_PT.F_CM.Input is
   
   ---------------
   -- From F_PT --
   ---------------

   function Navigation_Parameters return Navigation_Parameters_Type 
     renames MMS.F_PT.Input.Navigation_Parameters;

   function Navigation_Mode return Navigation_Mode_Type
     renames MMS.F_PT.Input.Navigation_Mode;
   
   function Navigation_Option return Navigation_Option_Type
     renames MMS.F_PT.Input.Navigation_Option;

   function Go return Boolean
     renames MMS.F_PT.Input.Go;   

   function Emergency_Landing return Boolean
     renames MMS.F_PT.Input.Emergency_Landing;

   function On_OFF_Push_Button return Boolean
     renames MMS.F_PT.Input.On_OFF_Push_Button;

   function Start_Push_Button return Boolean
     renames MMS.F_PT.Input.Start_Push_Button;

   function Mode_Switch return Navigation_Mode_Type
     renames MMS.F_PT.Input.Mode_Switch;

   function Bay_Switch return Bay_Switch_Type
     renames MMS.F_PT.Input.Bay_Switch; 

   function Payload_Mass return Payload_Mass_Type
     renames MMS.F_PT.Input.Payload_Mass;
   
   function USB_Key return Navigation_Parameters_Type
     renames MMS.F_PT.Input.USB_Key;
   
   function P return Distance_Type
     renames MMS.F_PT.Input.P;
   
   function P_Dot return Speed_Type
     renames MMS.F_PT.Input.P_Dot;
   
   function Q return Angle_Type
     renames MMS.F_PT.Input.Q;
   
   ---------------
   -- From F_MM --
   ---------------
   
   function Mission_Aborted return Boolean 
     renames MMS.F_PT.F_MM.Output.Mission_Aborted;
   
   function Mission_Complete return Boolean  
     renames MMS.F_PT.F_MM.Output.Mission_Complete;
   
   function Mission_Cancelled return Boolean
     renames MMS.F_PT.F_MM.Output.Mission_Cancelled;
   
   ---------------
   -- From F_EM --
   ---------------
   
   function Primary_Source return Source_Type
     renames MMS.F_PT.F_EM.Output.Primary_Source;
   
   function Secondary_Source return Source_Type
     renames MMS.F_PT.F_EM.Output.Secondary_Source;
   
   ---------------
   -- From F_FC --
   ---------------
   
   function Propulsion_Torque return Torque_Type
     renames MMS.F_PT.F_FC.Output.Propulsion_Torque;
   
   function Braking_Torque return Torque_Type
     renames MMS.F_PT.F_FC.Output.Braking_Torque;
   
end MMS.F_PT.F_CM.Input;
