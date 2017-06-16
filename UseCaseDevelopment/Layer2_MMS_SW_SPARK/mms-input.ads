package MMS.Input is

   ------------------------------------------------------
   -- Ground-based Mission Preparation and Supervision --
   ------------------------------------------------------

   function Navigation_Parameters return Navigation_Parameters_Type;

   function Navigation_Mode return Navigation_Mode_Type;

   function Navigation_Option return Navigation_Option_Type;

   function Go return Boolean;

   function Emergency_Landing return Boolean;

   --------------------------------------------------
   -- AV-based Mission Preparation (Control Panel) --
   --------------------------------------------------

   function On_OFF_Push_Button return Boolean;

   function Start_Push_Button return Boolean;

   function Mode_Switch return Navigation_Mode_Type;

   function Bay_Switch return Bay_Switch_Type;

   function Payload_Mass return Payload_Mass_Type;

   function USB_Key return Navigation_Parameters_Type;

   function Rotactor_1 return Rotactor_Type;

   function Rotactor_2 return Rotactor_Type;

   -------------------------
   -- Physical Parameters --
   -------------------------

   function P return Distance_Type;

   function P_Dot return Speed_Type;

   function Q return Altitude_Type;

end MMS.Input;
