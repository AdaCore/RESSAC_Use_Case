with Types; use Types;

package External with Abstract_State => (State with External => Async_Writers) is

   ------------------------------------------------------
   -- Ground-based Mission Preparation and Supervision --
   ------------------------------------------------------

   function Navigation_Parameters return Navigation_Parameters_Type_Option with
     Volatile_Function,
     Global => State;

   function Navigation_Mode return Navigation_Mode_Type_Option with
     Volatile_Function,
     Global => State;

   function Navigation_Option return Navigation_Option_Type_Option with
     Volatile_Function,
     Global => State;

   function Go return Boolean with
     Volatile_Function,
     Global => State;

   function Emergency_Landing return Boolean with
     Volatile_Function,
     Global => State;

   --------------------------------------------------
   -- AV-based Mission Preparation (Control Panel) --
   --------------------------------------------------

   function On_OFF_Push_Button return Boolean with
     Volatile_Function,
     Global => State;

   function Start_Push_Button return Boolean with
     Volatile_Function,
     Global => State;

   function Mode_Switch return Navigation_Mode_Type with
     Volatile_Function,
     Global => State;

   function Bay_Switch return Bay_Switch_Type with
     Volatile_Function,
     Global => State;

   function Rotactor_1 return Rotactor_Type with
     Volatile_Function,
     Global => State;

   function Rotactor_2 return Rotactor_Type with
     Volatile_Function,
     Global => State;

   function USB_Key return USB_Key_Type_Option with
     Volatile_Function,
     Global => State;

   -------------------------
   -- Physical Parameters --
   -------------------------

   function P return Distance_Type with
     Volatile_Function,
     Global => State;

   function P_Dot return Speed_Type with
     Volatile_Function,
     Global => State;

   function Q return Angle_Type with
     Volatile_Function,
     Global => State;

end External;
