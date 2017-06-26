with MMS.F_PT.F_CM.Input;

with Types; use Types;

package MMS.F_PT.F_CM.Output is

   -------------
   -- To F_PT --
   -------------

   function CP_Switches return CP_Switches_Type is
     (CP_Switches_Type'
        (Power      => MMS.F_PT.F_CM.Input.On_OFF_Push_Button, -- ???
         Mode       => MMS.F_PT.F_CM.Input.Mode_Switch,
         Bay        => MMS.F_PT.F_CM.Input.Bay_Switch,
         Start      => MMS.F_PT.F_CM.Input.Start_Push_Button,
         Rotactor_1 => 
            Rotactor_Type (MMS.F_PT.F_CM.Input.Payload_Mass / 10),
         Rotactor_2 => 
            Rotactor_Type (MMS.F_PT.F_CM.Input.Payload_Mass mod 10)));
   --  ??? Rotactors are computed from payload mass, which one is which?
      
   function CP_Displays return CP_Displays_Type is
     (CP_Displays_Type'
        (Ready            => True, --  This register is not loaded at increment 1
         Cancelled        => MMS.F_PT.F_CM.Input.Mission_Cancelled,
         Complete         => MMS.F_PT.F_CM.Input.Mission_Complete,
         Aborted          => MMS.F_PT.F_CM.Input.Mission_Aborted,
         Primary_Source   => MMS.F_PT.F_CM.Input.Primary_Source,
         Secondary_Source => MMS.F_PT.F_CM.Input.Secondary_Source));
   
   function Propulsion_Energy return Propulsion_Energy_Type is
     (Propulsion_Energy_Type'
        (Primary_Source_Capacity   => MMS.F_PT.F_CM.Input.Primary_Source,
         Secondary_Source_Capacity => MMS.F_PT.F_CM.Input.Secondary_Source));
   
   function Mission_Cancelled return Boolean
     renames MMS.F_PT.F_CM.Input.Mission_Cancelled;
   
   function Mission_Complete return Boolean
     renames MMS.F_PT.F_CM.Input.Mission_Complete;
   
   function Mission_Aborted return Boolean
   renames MMS.F_PT.F_CM.Input.Mission_Aborted;
   
   function Primary_Source return Source_Type
   renames MMS.F_PT.F_CM.Input.Primary_Source;
   
   function Secondary_Source return Source_Type
     renames MMS.F_PT.F_CM.Input.Secondary_Source;
   
   function Propulsion_Torque return Torque_Type
     renames MMS.F_PT.F_CM.Input.Propulsion_Torque;
   
   function Braking_Torque return Torque_Type
     renames MMS.F_PT.F_CM.Input.Braking_Torque;

   -------------
   -- To F_MM --
   -------------

   function Navigation_Parameters return Navigation_Parameters_Type 
     renames MMS.F_PT.F_CM.Input.Navigation_Parameters;

   function Navigation_Mode return Navigation_Mode_Type
     renames MMS.F_PT.F_CM.Input.Navigation_Mode;
   
   function Navigation_Option return Navigation_Option_Type
     renames MMS.F_PT.F_CM.Input.Navigation_Option;

   function Go return Boolean
     renames MMS.F_PT.F_CM.Input.Go;   

   function Emergency_Landing return Boolean
     renames MMS.F_PT.F_CM.Input.Emergency_Landing;

   function On_OFF_Push_Button return Boolean
     renames MMS.F_PT.F_CM.Input.On_OFF_Push_Button;

   function Start_Push_Button return Boolean
     renames MMS.F_PT.F_CM.Input.Start_Push_Button;

   function Mode_Switch return Navigation_Mode_Type
     renames MMS.F_PT.F_CM.Input.Mode_Switch;

   function Bay_Switch return Bay_Switch_Type
     renames MMS.F_PT.F_CM.Input.Bay_Switch;
   
   function USB_Key return Navigation_Parameters_Type
     renames MMS.F_PT.F_CM.Input.USB_Key; 

   function Payload_Mass_To_F_MM return Payload_Mass_Type
     renames MMS.F_PT.F_CM.Input.Payload_Mass;
   
   -------------
   -- To F_FC --
   -------------

   function P return Distance_Type renames MMS.F_PT.F_CM.Input.P;
   
   function P_Dot return Speed_Type renames MMS.F_PT.F_CM.Input.P_Dot;
   
   function Q return Angle_Type renames MMS.F_PT.F_CM.Input.Q;

   function Payload_Mass_To_F_FC return Payload_Mass_Type
     renames MMS.F_PT.F_CM.Input.Payload_Mass;
   
end MMS.F_PT.F_CM.Output;
