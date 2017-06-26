--  This package provides a wrapper above MMS.F_PT.F_MM.Behavior.Run which
--  is used to verify in SPARK that high level guarantees on F_MM are implied
--  by its behavioural specification.

with Types; use Types;

package MMS.F_PT.F_MM.Behavior.Guarantees with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);

   -----------------------------------
   -- High-Level Properties on F_MM --
   -----------------------------------

   function In_Take_Off_State return Boolean is
      (Power_State = On
       and then On_State = RUNNING
       and then Running_State = TAKE_OFF);

   function Energy_Check_Succeed return Boolean is
     (Power_State = ON
      and then On_State = INIT
      and then Energy_Compatible_With_Mission);

   function Energy_Check_Fail return Boolean is
     (Power_State = ON
      and then On_State = RUNNING
      and then Running_State = CRUISE
      and then not Energy_Compatible_With_Mission);

   function Mission_Cancelled return Boolean is
      (Power_State = On
       and then On_State = CANCELLED);

   -----------------------------------
   -- High-Level Garantees for F_MM --
   -----------------------------------

   procedure Run with
     Post =>

      --  6.6.3.A Viability guarantee: no take-off if energy aboard is
      --  incompatible with mission completion.

      (if In_Take_Off_State and then not In_Take_Off_State'Old then
         Energy_Check_Succeed'Old)

      --  6.6.3.B Any mission cancellation is signaled to CP and GS.

      and then
       (if Mission_Cancelled and then not Mission_Cancelled'Old then
           Mission_Cancellation_Signaled)

      --  6.6.3.2.A Missions cancelled for energy reasons can be proven
      --  infeasible.

      and then
       (if Mission_Cancelled and then not Mission_Cancelled'Old then
           Energy_Check_Fail'Old);

end MMS.F_PT.F_MM.Behavior.Guarantees;
