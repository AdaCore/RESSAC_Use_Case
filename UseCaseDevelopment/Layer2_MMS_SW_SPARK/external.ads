with Types; use Types;

package External with Abstract_State =>
  ((From_GS with External => Async_Writers),
   (From_CP with External => Async_Writers),
   (From_P_P with External => Async_Writers)) is

   ------------------------------------------------------
   -- Ground-based Mission Preparation and Supervision --
   ------------------------------------------------------

   function Navigation_Parameters return Navigation_Parameters_Type with
     Volatile_Function,
     Global => From_GS;

   function Navigation_Mode return Navigation_Mode_Type with
     Volatile_Function,
     Global => From_GS;

   function Navigation_Option return Navigation_Option_Type with
     Volatile_Function,
     Global => From_GS;

   function Go return Boolean with
     Volatile_Function,
     Global => From_GS;

   function Emergency_Landing return Boolean with
     Volatile_Function,
     Global => From_GS;

   --------------------------------------------------
   -- AV-based Mission Preparation (Control Panel) --
   --------------------------------------------------

   function On_OFF_Push_Button return Boolean with
     Volatile_Function,
     Global => From_CP;

   function Start_Push_Button return Boolean with
     Volatile_Function,
     Global => From_CP;

   function Mode_Switch return Navigation_Mode_Type with
     Volatile_Function,
     Global => From_CP;

   function Bay_Switch return Bay_Switch_Type with
     Volatile_Function,
     Global => From_CP;

   function Payload_Mass return Payload_Mass_Type with
     Volatile_Function,
     Global => From_CP;

   function USB_Key return Navigation_Parameters_Type with
     Volatile_Function,
     Global => From_CP;

   -------------------------
   -- Physical Parameters --
   -------------------------

   function P return Distance_Type with
     Volatile_Function,
     Global => From_P_P;

   function P_Dot return Speed_Type with
     Volatile_Function,
     Global => From_P_P;

   function Q return Angle_Type with
     Volatile_Function,
     Global => From_P_P;

end External;
