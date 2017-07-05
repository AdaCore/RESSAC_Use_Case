--  This package provides a Run procedure which simulates execution of the
--  main loop of F_MM and is used to verify in SPARK that high level guarantees
--  on F_MM are implied by its behavioural specification.

with Types; use Types;

package MMS.F_PT.F_MM.Behavior.Guarantees with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);

   -----------------------------------
   -- High-Level Properties on F_MM --
   -----------------------------------

   function In_Take_Off_State return Boolean is
      (Power_State = On
       and then On_State = RUNNING
       and then Running_State = TAKE_OFF)
   with Global => Private_State;

   function Mission_Aborted return Boolean is
      (Power_State = On
       and then On_State = ABORTED)
   with Global => Private_State;

   function Mission_Cancelled return Boolean is
      (Power_State = On
       and then On_State = INIT
       and then Init_State = CANCELLED)
   with Global => Private_State;

   function Initial_Energy_Test_Succeeded return Boolean with Ghost;

   function In_Flight_Energy_Test_Failed return Boolean with Ghost;

   -----------------------------------
   -- High-Level Garantees for F_MM --
   -----------------------------------

   procedure Run with
     Post =>

      --  6.6.3.A Viability guarantee: no take-off if energy aboard is
      --  incompatible with mission completion.

      (if In_Take_Off_State and then not In_Take_Off_State'Old then
         Initial_Energy_Test_Succeeded)

      --  6.6.3.B Any mission cancellation is signaled to CP and GS.

      and then
       (if Mission_Aborted then Mission_Aborted_Signaled)
      and then
       (if Mission_Cancelled then Mission_Cancelled_Signaled)

      --  6.6.3.2.A Missions cancelled for energy reasons can be proven
      --  infeasible.

      and then
       (if Mission_Aborted and then not Mission_Aborted'Old
          and then Aborted_For_Energy_Reasons
        then In_Flight_Energy_Test_Failed)

      --  6.9.3.2.C When A mode is set on CP, the navigation options/parameters
      --  are that of USB key or initialization is not complete.

      and then
       (if Power_On
          and then Navigation_Mode_From_CP = A
          and then Mission_Parameters_Defined
        then
          USB_Key_Present
        and then Operating_Mode = Operating_Mode_From_CP
        and then Navigation_Parameters = Navigation_Parameters_From_USB_Key);

end MMS.F_PT.F_MM.Behavior.Guarantees;
