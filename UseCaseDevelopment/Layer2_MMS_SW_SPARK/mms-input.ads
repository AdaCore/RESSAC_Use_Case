with Types; use Types;
with External;

package MMS.Input is

   ------------------------------------------------------
   -- Ground-based Mission Preparation and Supervision --
   ------------------------------------------------------

   function Navigation_Parameters return Navigation_Parameters_Type
     renames External.Navigation_Parameters;

   function Navigation_Mode return Navigation_Mode_Type
     renames External.Navigation_Mode;

   function Navigation_Option return Navigation_Option_Type
     renames External.Navigation_Option;

   function Go return Boolean renames External.Go;

   function Emergency_Landing return Boolean renames External.Emergency_Landing;

   --------------------------------------------------
   -- AV-based Mission Preparation (Control Panel) --
   --------------------------------------------------

   function On_OFF_Push_Button return Boolean
     renames External.On_OFF_Push_Button;

   function Start_Push_Button return Boolean
     renames External.Start_Push_Button;

   function Mode_Switch return Navigation_Mode_Type
     renames External.Mode_Switch;

   function Bay_Switch return Bay_Switch_Type
     renames External.Bay_Switch;

   function Payload_Mass return Payload_Mass_Type
     renames External.Payload_Mass;

   function USB_Key return Navigation_Parameters_Type
     renames External.USB_Key;

   -------------------------
   -- Physical Parameters --
   -------------------------

   function P return Distance_Type renames External.P;

   function P_Dot return Speed_Type renames External.P_Dot;

   function Q return Angle_Type renames External.Q;

end MMS.Input;
